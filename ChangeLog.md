# Changelog for pretty-relative-time

## Unreleased changes

## [0.2.0.0] - 2020-02-12

### Changed

Fixed a bug like this:

```
 approximateDaysPerMonth :: Integral a => a
-approximateDaysPerMonth = 30
+approximateDaysPerMonth = 31
```

