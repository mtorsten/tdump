# Design Document

## Overview

The tdump program is structured as a command-line utility with modular components for argument parsing, data reading, and formatted output generation. The design emphasizes clean separation of concerns and follows Free Pascal best practices for console applications.

## Architecture

The program follows a layered architecture:

1. **Command Line Interface Layer**: Handles argument parsing and validation
2. **Configuration Layer**: Manages program settings and options
3. **Data Input Layer**: Abstracts file and standard input reading
4. **Processing Layer**: Handles data formatting and output generation
5. **Output Layer**: Manages console output and verbose information

```
┌─────────────────┐
│   Main Program  │
└─────────┬───────┘
          │
┌─────────▼───────┐
│ Command Parser  │
└─────────┬───────┘
          │
┌─────────▼───────┐
│  Configuration  │
└─────────┬───────┘
          │
┌─────────▼───────┐
│   Data Reader   │
└─────────┬───────┘
          │
┌─────────▼───────┐
│ Hex Formatter   │
└─────────┬───────┘
          │
┌─────────▼───────┐
│    Output       │
└─────────────────┘
```

## Components and Interfaces

### TDumpConfig Record
Stores all configuration options parsed from command line:
```pascal
type
  TOutputMode = (omByte, omWord, omDWord);
  
  TDumpConfig = record
    Count: Integer;
    OutputMode: TOutputMode;
    Verbose: Boolean;
    InputFile: string;
    UseStdIn: Boolean;
    ShowHelp: Boolean;
  end;
```

### Command Line Parser
- **ParseCommandLine(var Config: TDumpConfig): Boolean**
  - Parses all command line arguments
  - Validates option combinations
  - Returns false on error, true on success
  - Handles both short (-c) and long (--count) option formats

### Data Reader Interface
- **OpenInput(const FileName: string; UseStdIn: Boolean): Boolean**
  - Opens file or prepares standard input
  - Returns success status
- **ReadNextChunk(var Buffer: array of Byte; MaxBytes: Integer): Integer**
  - Reads up to MaxBytes into buffer
  - Returns actual bytes read (0 = EOF)
- **GetFileSize(): Int64**
  - Returns file size in bytes (-1 if unknown/stdin)
- **CloseInput()**
  - Closes file handles

### Hex Formatter
- **FormatHexLine(const Buffer: array of Byte; Count: Integer; Offset: Int64; Mode: TOutputMode): string**
  - Formats a single line of hex output
  - Handles byte/word/dword grouping
  - Includes offset, hex data, and ASCII representation
- **FormatVerboseLine(const FileName: string; FileSize: Int64; LineCount: Integer): string**
  - Creates header/footer lines for verbose mode
  - Includes ISO datetime, size, line count, and filename

## Data Models

### Buffer Management
- Use fixed-size byte array buffer (4096 bytes) for efficient file reading
- Process data in chunks to handle large files without memory issues
- Maintain running offset counter for accurate position display

### Output Line Structure
```
OOOOOOOO  HH HH HH HH HH HH HH HH HH HH HH HH HH HH HH HH  AAAAAAAAAAAAAAAA
```
Where:
- O = 8-digit hexadecimal offset
- H = Hexadecimal byte data (grouped by mode)
- A = ASCII representation

### Grouping Modes
- **Byte Mode**: `01 02 03 04` (2 digits + space)
- **Word Mode**: `0102 0304` (4 digits + space)  
- **DWord Mode**: `01020304` (8 digits + space)

## Error Handling

### Command Line Errors
- Invalid options: Display error message and usage
- Invalid count values: Show error and valid range
- Conflicting options: Use last specified option

### File Access Errors
- File not found: Display clear error message with filename
- Permission denied: Show appropriate error message
- I/O errors during reading: Display error and exit gracefully

### Data Processing Errors
- Partial reads: Handle gracefully, format available data
- Memory allocation: Use stack-based buffers to avoid heap issues

## Testing Strategy

### Unit Testing Focus
- Command line parsing with various option combinations
- Hex formatting functions with different modes and data patterns
- Buffer handling and offset calculations
- Verbose output formatting

### Integration Testing
- End-to-end testing with sample binary files
- Standard input processing with piped data
- Error scenarios (missing files, invalid options)
- Output verification against expected hex dump format

### Test Data
- Small binary files with known content
- Files with various sizes (empty, single byte, multiple of count, partial last line)
- Files containing printable and non-printable characters
- Large files to test chunked reading

## Implementation Notes

### Free Pascal Specific Considerations
- Use `ParamStr()` and `ParamCount` for command line access
- Utilize `FileExists()` and `FileSize()` for file operations
- Implement proper file handle management with `AssignFile`, `Reset`, `BlockRead`
- Use `DateTimeToStr()` with ISO format for verbose timestamps
- Handle both Windows and Unix path separators appropriately

### Performance Considerations
- Buffer I/O operations to minimize system calls
- Pre-calculate string lengths for efficient output formatting
- Use efficient string concatenation methods
- Minimize memory allocations in main processing loop

### Cross-Platform Compatibility
- Use Free Pascal's cross-platform file I/O routines
- Ensure consistent path handling across platforms
- Treat all input as binary data - line endings (LF, CR, CRLF) are displayed as hex values and non-printable characters