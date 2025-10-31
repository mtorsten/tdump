# Implementation Plan

- [x] 1. Set up program structure and basic types
  - Create the main program file with proper Free Pascal structure
  - Define TOutputMode enumeration and TDumpConfig record
  - Set up basic program skeleton with main procedure
  - _Requirements: 5.1, 7.3_

- [x] 2. Implement command line argument parsing
  - [x] 2.1 Create ParseCommandLine function
    - Write function to parse short and long form options (-c/--count, -w/--word, -d/--dword, -v/--verbose, -h/--help)
    - Handle count parameter validation and integer parsing
    - Implement filename detection as last non-option argument
    - _Requirements: 5.1, 5.2, 5.4_
  
  - [x] 2.2 Add help display functionality
    - Create ShowHelp procedure to display usage information
    - Include all option descriptions and example usage patterns
    - Handle help option processing and program exit
    - _Requirements: 6.1, 6.2, 6.3, 6.4_
  
  - [x] 2.3 Implement error handling for invalid arguments
    - Add validation for conflicting options and invalid count values
    - Display appropriate error messages for invalid options
    - Ensure graceful handling of malformed command lines
    - _Requirements: 5.3, 5.4, 5.5_

- [x] 3. Create data input handling system
  - [x] 3.1 Implement file and standard input management
    - Write OpenInput function to handle file opening and stdin preparation
    - Create GetFileSize function for file size determination
    - Implement CloseInput procedure for proper resource cleanup
    - _Requirements: 1.1, 1.2, 7.1, 7.2_
  
  - [x] 3.2 Add chunked data reading functionality
    - Create ReadNextChunk function with buffer management
    - Implement efficient file reading with 4096-byte buffer
    - Handle end-of-file and partial read scenarios
    - _Requirements: 1.1, 1.2_
  
  - [x] 3.3 Implement file access error handling
    - Add error checking for file existence and permissions
    - Create appropriate error messages for file access failures
    - Ensure graceful program termination on I/O errors
    - _Requirements: 5.5_

- [x] 4. Build hexadecimal formatting engine
  - [x] 4.1 Create core hex formatting function
    - Write FormatHexLine function for single line output
    - Implement offset formatting as 8-digit hexadecimal
    - Add ASCII character filtering (printable vs period replacement)
    - _Requirements: 1.3, 1.4, 1.5_
  
  - [x] 4.2 Add byte grouping modes
    - Implement byte mode formatting (2 hex digits per byte)
    - Add word mode formatting (4 hex digits per group)
    - Create dword mode formatting (8 hex digits per group)
    - Handle proper spacing between groups
    - _Requirements: 3.1, 3.2, 3.3, 3.4_
  
  - [x] 4.3 Implement count validation and adjustment
    - Add count rounding logic for word and dword modes
    - Ensure minimum count values (1 for byte, 2 for word, 4 for dword)
    - Handle count parameter validation during parsing
    - _Requirements: 2.2, 2.3, 2.4, 2.5_

- [x] 5. Add verbose output functionality
  - [x] 5.1 Create verbose header/footer formatting
    - Write FormatVerboseLine function with ISO datetime
    - Include file size, line count calculation, and full file path
    - Handle different formatting for file vs stdin input
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 4.6_
  
  - [x] 5.2 Implement verbose output logic
    - Add header display for file input with verbose option
    - Create footer display for both file and stdin with verbose option
    - Calculate and display correct line count based on file size and count parameter
    - _Requirements: 4.1, 4.2, 4.7_

- [x] 6. Integrate main processing loop
  - [x] 6.1 Create main data processing workflow
    - Write main loop to read data chunks and format output
    - Maintain running offset counter for accurate position display
    - Handle partial last lines and end-of-file scenarios
    - _Requirements: 1.1, 1.2, 1.3, 1.4, 1.5_
  
  - [x] 6.2 Wire together all components
    - Connect command line parsing to configuration
    - Link data reading with hex formatting
    - Integrate verbose output with main processing flow
    - _Requirements: 7.1, 7.2, 7.3_
  
  - [x] 6.3 Add comprehensive error handling
    - Implement try-except blocks for I/O operations
    - Add proper cleanup in error scenarios
    - Ensure consistent error message formatting
    - _Requirements: 5.5_

- [x] 7. Create test validation
  - [x] 7.1 Write unit tests for core functions
    - Test command line parsing with various option combinations
    - Validate hex formatting functions with different modes
    - Test buffer handling and offset calculations
    - _Requirements: 5.1, 1.3, 1.4_
  
  - [x] 7.2 Add integration tests
    - Test end-to-end functionality with sample binary files
    - Validate standard input processing
    - Test error scenarios and edge cases
    - _Requirements: 1.1, 1.2, 7.1, 7.2_