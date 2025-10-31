# Requirements Document

## Introduction

The tdump program is a hexadecimal dump utility written in Free Pascal that reads binary data from files or standard input and displays it in a formatted hexadecimal representation alongside printable ASCII characters. The program provides flexible output formatting options and supports various byte grouping modes.

## Glossary

- **tdump**: The hexadecimal dump utility program
- **Hex_Output_System**: The component responsible for formatting and displaying hexadecimal data
- **File_Reader**: The component that handles reading data from files or standard input
- **Command_Parser**: The component that processes command-line arguments and options
- **Offset**: The position in bytes from the beginning of the input data
- **Count**: The number of bytes to display per output line
- **Word_Mode**: 16-bit grouping mode displaying 4 hexadecimal digits per group
- **DWord_Mode**: 32-bit grouping mode displaying 8 hexadecimal digits per group
- **Byte_Mode**: Default 8-bit mode displaying 2 hexadecimal digits per group
- **Printable_Character**: ASCII characters in the range 32-126 that can be displayed as text
- **Non_Printable_Character**: ASCII characters outside the printable range, displayed as periods

## Requirements

### Requirement 1

**User Story:** As a developer, I want to dump binary file contents in hexadecimal format, so that I can analyze file structure and data patterns.

#### Acceptance Criteria

1. WHEN a filename is provided as the last command-line argument, THE tdump SHALL read and process the entire file
2. WHEN no filename is provided, THE tdump SHALL read data from standard input
3. THE tdump SHALL display each byte as two hexadecimal digits in uppercase format
4. THE tdump SHALL display the offset position as an 8-digit hexadecimal number at the beginning of each line
5. THE tdump SHALL display printable ASCII characters (32-126) in the text portion and replace non-printable characters with periods

### Requirement 2

**User Story:** As a user, I want to control the number of bytes displayed per line, so that I can adjust the output format to my needs.

#### Acceptance Criteria

1. WHEN the -c or --count option is specified with an integer, THE tdump SHALL display that number of bytes per line
2. WHEN no count option is specified, THE tdump SHALL default to 16 bytes per line
3. IF the count value is not divisible by 2 in Word_Mode, THEN THE tdump SHALL round down to the nearest valid value
4. IF the count value is not divisible by 4 in DWord_Mode, THEN THE tdump SHALL round down to the nearest valid value
5. THE tdump SHALL ensure the minimum count is 1 for Byte_Mode, 2 for Word_Mode, and 4 for DWord_Mode

### Requirement 3

**User Story:** As a user, I want to group hexadecimal output in different word sizes, so that I can view data in the most appropriate format for my analysis.

#### Acceptance Criteria

1. WHEN the -w or --word option is specified, THE tdump SHALL group hexadecimal output as 16-bit words with 4 digits per group
2. WHEN the -d or --dword option is specified, THE tdump SHALL group hexadecimal output as 32-bit double words with 8 digits per group
3. WHEN neither word nor dword options are specified, THE tdump SHALL default to byte mode with 2 digits per group
4. THE tdump SHALL separate each group with a single space character
5. IF both word and dword options are specified, THEN THE tdump SHALL use the last specified option

### Requirement 4

**User Story:** As a user, I want verbose output with file information, so that I can see metadata about the dumped data.

#### Acceptance Criteria

1. WHEN the -v or --verbose option is specified for file input, THE tdump SHALL display a header line before the hex dump
2. WHEN the -v or --verbose option is specified for file input, THE tdump SHALL display a footer line after the hex dump
3. THE tdump SHALL include current date-time in ISO format in header and footer lines
4. THE tdump SHALL include file size in bytes in header and footer lines
5. THE tdump SHALL include calculated number of output lines in header and footer lines
6. THE tdump SHALL include the full file path in header and footer lines
7. WHEN input is from standard input with verbose option, THE tdump SHALL display only the footer line

### Requirement 5

**User Story:** As a user, I want proper command-line argument parsing, so that I can use the program with various option combinations.

#### Acceptance Criteria

1. THE tdump SHALL accept short form options (-c, -w, -d, -v, -h) and long form options (--count, --word, --dword, --verbose, --help)
2. THE tdump SHALL treat the last non-option argument as the filename
3. WHEN invalid options are provided, THE tdump SHALL display an error message and usage information
4. WHEN the count option is provided without a valid integer, THE tdump SHALL display an error message
5. THE tdump SHALL handle file access errors gracefully with appropriate error messages

### Requirement 6

**User Story:** As a user, I want to see help information, so that I can understand how to use the program correctly.

#### Acceptance Criteria

1. WHEN the -h or --help option is specified, THE tdump SHALL display usage information and program options
2. WHEN the -h or --help option is specified, THE tdump SHALL exit without processing any input data
3. THE tdump SHALL include descriptions of all available options in the help output
4. THE tdump SHALL include example usage patterns in the help output

### Requirement 7

**User Story:** As a user, I want the program to work without any parameters, so that I can use it in pipes and simple scenarios.

#### Acceptance Criteria

1. WHEN tdump is invoked without any parameters, THE tdump SHALL read from standard input
2. WHEN tdump is invoked without any parameters, THE tdump SHALL write output to standard output
3. WHEN tdump is invoked without any parameters, THE tdump SHALL use default formatting options (16 bytes per line, byte mode, no verbose output)