# pretty-relative-time

The `pretty-relative-time` library describes dates and times in the past and future in a nice, human readable, familiar feeling way.

## Why

Often when showing dates to the user, it is quite hard to let them get a feeling of when a certain date or time took place. It would be a lot nicer of a user experience if we actually saw the date described in a similar way to how we as humans describe dates. For example, compare `1946-07-14` to `1946-07-14 (73 years ago)` or `2019-12-31` to `2019-12-31 (in 4 months)`. This library does that.

## How

### Pieces of the puzzle

When dealing with time in Haskell, we usually use [the `time` library](https://hackage.haskell.org/package/time). Timestamps in that library are represented using `UTCTime`. When it comes to timestamps on the granularity of a single day, we use `Day`.

To represent relative times in a pretty way, we'll be building a string based on the current time and the time in question. We could try to make a function that is always based on the latest 'now', but that would require `IO`.

```haskell
prettyRelativeTime
  :: UTCTime -- ^ Now
  -> UTCTime -- ^ The time in question
  -> String

prettyRelativeDay
  :: Day -- ^ Today
  -> Day -- ^ The day in question
  -> String
```

This API is nice, but implementing these functions all in one go becomes very complex very quickly. Instead, we'll split up the work into two pieces. The first part derives a more complex structure from a difference in time. This structure will contain the information that's needed to render a pretty string in a more ready-to-use way. The second part is about actually using that structure to render a nice description of the relative time. An interesting benefit of this separation is that you could have different rendering functions for different languages.

### The pretty-relative solution

As an intermediate structure, we will use a type that represents the difference in time exactly. This will come in handy during testing.

```haskell
data DaysAgo =
  DaysAgo
    { daysAgoSign :: !Ordering
    , daysAgoYears :: !Integer
    , daysAgoMonths :: !Integer
    , daysAgoWeeks :: !Integer
    , daysAgoDays :: !Integer
    } 
```

If you are used to writing Haskell, you will immediately feel uncomfortable with such a structure. Indeed, it leaves a lot of redundancy. There are possibly many ways to represent the same number of days.

That is where validity constraints come in. We describe enough invariants to make sure that there is a one to one correspondence between an `Integer` number of days and `DaysAgo`:

```haskell
instance Validity DaysAgo where
  validate da@DaysAgo {..} =
    mconcat
      [ genericValidate da
      , check
          (case daysAgoSign of
             EQ -> and [daysAgoDays == 0, daysAgoWeeks == 0, daysAgoMonths == 0, daysAgoYears == 0]
             _ -> any (> 0) [daysAgoDays, daysAgoWeeks, daysAgoMonths, daysAgoYears])
          "the sign makes sense"
      , check (daysAgoYears >= 0) "years are positive"
      , check
          (daysAgoDays + 7 * daysAgoWeeks + 30 * daysAgoMonths < 356)
          "days, weeks and months do not sum to a year"
      , check (daysAgoMonths < 12) "months < 12"
      , check (daysAgoMonths >= 0) "months are positive"
      , check
          (daysAgoDays + 7 * daysAgoWeeks < 30)
          "days and weeks do not sum to a month"
      , check (daysAgoWeeks < 5) "weeks < 5"
      , check (daysAgoWeeks >= 0) "weeks are positive"
      , check (daysAgoDays < 7) "days < 7"
      , check (daysAgoDays >= 0) "days are positive"
      ]
```

These constraints say the following:

- `genericValidate da`: All the pieces of `da` must be valid.
- The sign describes whether the relative time was in the future or in the past.
- All of the `Integer`s are positive.
- The number of months is 11 or fewer.
- The number of weeks is 4 or fewer.
- The number of days is 6 or fewer.
- There is no other way to describe a week, a month or a year than using the respective fields.

We will use testing to make sure that these validity constraints are never violated.

Note that we use some potentially objectionable constants. Indeed, it is not true that there are 30 days in every month, or 356 days in every year. However, this does not actually matter in this case, because we will use this structure to show strings that approximate times. For example, when we show `"2 months"` to a user, it does not matter whether that was 60 days, 62 days, or even 75 days.

Once we have decided on a type like this, we can write a `daysAgo :: Integer -> DaysAgo` function. To show that this function must be bijective, we will also write its inverse: `daysAgoToDays :: DaysAgo -> Integer`. The actual implementation of this function is complex, but also entirely uninteresting. It is left as an exercise to the reader.

The next step is to render a `DaysAgo` value in a nice way. It is entirely arbitrary, but we have chosen to output only the most significant part of the `DaysAgo` value. So if a date is three years and five months ago, we will only output `"3 years ago"`. If it is in five months and three weeks then we will only render `"in 5 months"`.

Again, the actual implementation of this function is complex but not necessarily interesting. The one interesting bit is the following function to produce a singular or a plural noun for a number:

```haskell
plural :: Integral a => a -> String -> String -> String
plural 1 sing _ = sing
plural _ _ plur = plur
```

You use it like this: `plural timeAgoSeconds "second" "seconds"`. (This is when I realised that zero is plural in English. Not so interestingly, that means that I am a multi-millionaire.)

The relative timestamps part of the library works in exactly the same way and will be left as an exercise to the reader.

### Testing the implementation

The testing part of this project is actually the more interesting part. The following two functions mostly ensure that the daysAgo implementation works:

```haskell
  describe "daysAgo" $ do
    it "produces valid TimeAgos" $ producesValidsOnValids daysAgo
    it "is the inverse of daysAgoToDays" $ inverseFunctionsOnValid daysAgo daysAgoToDays
  describe "daysAgoToDays" $ do
    it "produces valid results" $ producesValidsOnValids daysAgoToDays
    it "is the inverse of daysAgo" $ inverseFunctionsOnValid daysAgoToDays daysAgo
```

These tests caught every mistake that I made during development (so far). Usually when these tests find some problem it is not necessarily simple to figure out what the problem is or how to fix it. That is when we write more granular tests like the following:

```haskell
    it "is the inverse of daysAgo for just days" $
      inverseFunctionsOnGen
        daysAgoToDays
        daysAgo
        (((\d -> DaysAgo GT 0 d 0 0) <$> genValid) `suchThat` isValid)
        (const [])
```

At this point I am confident that the `daysAgo` function works, but the `prettyRelativeDay` days does not lend itself to property testing so easily. So we will just write unit tests, one for every piece of expected output:

```haskell
    it "produces valid Strings" $ producesValidsOnValids prettyRelativeDay
    let i da s = it (unwords ["Renders", show da, "as", show s]) $ prettyRelativeDay da `shouldBe` s
    describe "renders these simple examples well" $ do
      i (DaysAgo GT 5 0 0 0) "5 years ago"
      i (DaysAgo GT 0 4 0 0) "4 months ago"
      i (DaysAgo GT 0 0 3 0) "3 weeks ago"
      i (DaysAgo GT 0 0 0 2) "2 days ago"
      i (DaysAgo GT 0 0 0 1) "yesterday"
      i (DaysAgo EQ 0 0 0 0) "today"
      i (DaysAgo LT 0 0 0 1) "tomorrow"
      i (DaysAgo LT 0 0 0 2) "in 2 days"
      i (DaysAgo LT 0 0 3 0) "in 3 weeks"
      i (DaysAgo LT 0 4 0 0) "in 4 months"
      i (DaysAgo LT 5 0 0 0) "in 5 years"
    describe "handles singular nouns well" $ do
      i (DaysAgo GT 1 0 0 0) "1 year ago"
      i (DaysAgo GT 0 1 0 0) "1 month ago"
      i (DaysAgo GT 0 0 1 0) "1 week ago"
      i (DaysAgo GT 0 0 0 1) "yesterday"
      i (DaysAgo LT 0 0 0 1) "tomorrow"
      i (DaysAgo LT 0 0 1 0) "in 1 week"
      i (DaysAgo LT 0 1 0 0) "in 1 month"
      i (DaysAgo LT 1 0 0 0) "in 1 year"
```

Note:

- `"yesterday"`, `"today"` and `"tomorrow"` are tested separately.
- Each level of granularity is tested.
- Both singular and plural are tested for each of the levels of granularity.
- For plurals, a different plural is used every time to make it less likely that there is a problem that only shows up for specific numbers.
- There are no tests for more complicated values of DaysAgo where more than one part is nonzero. Frankly, that is because this library is not going to be mission-critical and I could not be bothered to go that far with the testing.

## References

The `pretty-relative-time` library is available [on Hackage](https://hackage.haskell.org/package/pretty-relative-time). Mergeful originated in the work on [Smos](https://smos.cs-syd.eu/), [Intray](https://intray.cs-syd.eu/) and [Tickler](https://tickler.cs-syd.eu/). This post is part of an effort to encourage contributions to [Smos](https://github.com/NorfairKing/smos). The simplest contribution could be to just try out smos and provide feedback on the experience. [Smos](https://github.com/NorfairKing/smos) is a purely functional semantic forest editor of a subset of YAML that is intended to replace Emacs' Org-mode for Getting Things Done.

