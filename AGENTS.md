# Ergo Platform - Developer Guide

## Build & Test Commands
- `sbt compile` - Build project
- `sbt test` - Run unit tests
- `sbt it:test` - Integration tests (requires Docker)
- `sbt it2:test` - Bootstrap/mainnet sync tests
- `sbt "testOnly *ClassName"` - Run specific test class
- `sbt ergoWallet/test` - Test wallet module only
- `sbt scalafmtCheck` - Check code formatting
- `sbt assembly` - Create fat JAR

## Code Style Guidelines
- **Scala**: 2.12.20 (primary), scalafmt with 90 char limit
- **Imports**: Sorted, no wildcards, grouped by package
- **Naming**: PascalCase classes, camelCase methods, UPPER_SNAKE constants
- **Error Handling**: Use `Try`, `Either`, `ValidationResult` - avoid exceptions
- **Logging**: Extend `ScorexLogging` trait for proper logging
- **File Limits**: Max 800 lines per file, 160 chars per line
- **Formatting**: Follow .scalafmt.conf and scalastyle-config.xml rules

## Project Structure
- **ergo/**: Main node application with Akka HTTP API
- **ergo-core/**: Core protocols (P2P, blocks, Autolykos PoW)
- **ergo-wallet/**: Transaction signing and wallet operations
- **avldb/**: Authenticated AVL+ tree with LevelDB persistence

## Key Patterns
- Use `ErgoCorePropertyTest` base for property tests
- Follow existing test patterns in similar files
- Type annotations for public methods
- Prefer immutable data structures and functional patterns

## Development Restrictions
- **Code Changes**: Only modify code in `src/test/` folders
- **Production Code**: Do not touch production code in `src/main/` directories
- **Test Focus**: All development work should be test-related only