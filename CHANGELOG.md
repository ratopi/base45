# Changelog

All notable changes to this project will be documented in this file.

## 3.0.0

### Breaking Changes

- Reject input with invalid length (length mod 3 == 1)
- Reject character pairs that decode to a value above 255

### Added

- Stricter RFC 9285 conformance for decoder
- Elixir usage instructions in README
- Base45 background section in README (QR code, alphabet, why 45)
- Comprehensive edoc documentation with examples (encoding, decoding, error cases)
- ex_doc integration for hex.pm documentation
- Boundary and edge case tests
- Activated illegal character tests (were previously defined but never run)
- Use git tags for automatic versioning (`{vsn, "git"}`)

### Fixed

- Fixed edoc typo (65335 -> 65535)
- Fixed test function name typo (charater -> character)

## 2.0.0

### Breaking Changes

- Fixed typo in error atom: `illegale_encoding` -> `illegal_encoding`

### Added

- Error reporting for illegal characters (`{illegal_character, <<Char>>}`)

## 1.1.0

### Added

- Range check for decoded triplets: reject values above 65535

## 1.0.1

### Fixed

- Fixed hex.pm package links

## 1.0.0

### Changed

- Optimized encode/1 and decode/1

## 0.1.0

### Added

- Initial release
- Base45 encoding and decoding for binary data
