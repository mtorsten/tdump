# tdump - Hexadecimal Dump Utility

A command-line hexadecimal dump utility written in Free Pascal, similar to `hexdump` or `xxd`. This tool displays binary file contents in hexadecimal format with ASCII representation.

## Features

- **Multiple Output Modes**: Byte, word (16-bit), and dword (32-bit) grouping
- **Flexible Display**: Configurable bytes per line
- **Standard Input Support**: Read from stdin or files
- **Verbose Mode**: Display file information headers and footers
- **Error Handling**: Comprehensive error reporting and validation
- **Cross-platform**: Works on Linux, macOS, and Windows

## Installation

### Prerequisites
- Free Pascal Compiler (fpc) 3.0 or later

### Building
```bash
make
```

Or compile manually:
```bash
fpc tdump.pas
```

## Usage

```
tdump [options] [filename]
```

### Options

- `-c, --count N`     Number of bytes to display per line (default: 16)
- `-w, --word`        Group output as 16-bit words (4 hex digits per group)
- `-d, --dword`       Group output as 32-bit dwords (8 hex digits per group)
- `-v, --verbose`     Display file information header and footer
- `-h, --help`        Show help information

### Examples

```bash
# Dump a binary file with default settings
tdump file.bin

# Display 8 bytes per line
tdump -c 8 file.bin

# Word mode with 16 bytes per line
tdump -w -c 16 file.bin

# Verbose output with file information
tdump -v file.bin

# Read from standard input
cat file.bin | tdump
echo "Hello World" | tdump

# Use redirection
tdump < file.bin
```

### Output Format

```
OFFSET    HEX_DATA                                         ASCII_REPRESENTATION
00000000  48 65 6C 6C 6F 20 57 6F 72 6C 64 21 0A 00 FF 7F  Hello World!....
```

- **OFFSET**: 8-digit hexadecimal file offset
- **HEX_DATA**: Hexadecimal representation of bytes (grouped by mode)
- **ASCII_REPRESENTATION**: Printable ASCII characters (32-126), others shown as '.'

## Testing

The project includes comprehensive unit and integration tests:

```bash
# Run all tests
./run_all_tests.sh

# Run unit tests only
./test_tdump

# Run integration tests only
./run_integration_tests.sh
```

## Development

### Project Structure

```
├── tdump.pas              # Main program source
├── Makefile              # Build configuration
├── test_tdump.pas        # Unit tests
├── test_integration.pas  # Integration test source (unused)
├── run_integration_tests.sh  # Integration test runner
├── run_all_tests.sh      # All tests runner
└── .kiro/specs/          # Design specifications
    ├── requirements.md   # Feature requirements
    ├── design.md        # Technical design
    └── tasks.md         # Implementation tasks
```

### Requirements

The utility implements the following key requirements:

1. **File Processing**: Read and process binary files of any size
2. **Standard Input**: Support reading from stdin with proper handling
3. **Hex Formatting**: Display data in hexadecimal with configurable grouping
4. **ASCII Display**: Show printable characters alongside hex data
5. **Error Handling**: Comprehensive error reporting and graceful failure
6. **Command Line**: Full option parsing with short and long forms
7. **Performance**: Efficient buffered reading for large files

### Architecture

- **Modular Design**: Separate functions for parsing, formatting, and I/O
- **Configuration-Driven**: Central configuration record for all options
- **Error Resilient**: Proper error handling and resource cleanup
- **Memory Efficient**: Fixed-size buffers and streaming processing
- **Cross-Platform**: Uses standard Pascal I/O routines

## License

This project is open source. See the license file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

## Changelog

### v1.0.0
- Initial release
- Basic hexdump functionality
- Multiple output modes (byte, word, dword)
- Standard input support
- Verbose mode
- Comprehensive test suite