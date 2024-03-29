# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)

## [0.4.0.0] - 2022-08-16
### Added
- EJDB2IDObject class for object id utility
- JBL.FromJBL and JBL.ToJBL classes for objects deselization and serialization
- QSem for accessing the C API
### Changed
- the way data is deserialized and serialized
### Removed
- Aeson support for deselization and serialization

## [0.3.0.1] - 2020-12-22
### Changed
- base dependecy updated to `base >=4.12 && <4.14`
### Added
- BindM type exposed

## [0.3.0.0] - 2020-05-01
### Changed
- Query is now a data type with binding as Monad
- Query is available via Database.EJDB2 module

## [0.2.0.0] - 2020-04-19
### Added
- fold function to reduce a query result
