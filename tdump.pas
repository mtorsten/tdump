program tdump;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

type
  { Enumeration for different output modes }
  TOutputMode = (omByte, omWord, omDWord);
  
  { Configuration record to store all program options }
  TDumpConfig = record
    Count: Integer;           // Number of bytes per line
    OutputMode: TOutputMode;  // Byte, word, or dword grouping
    Verbose: Boolean;         // Enable verbose output
    InputFile: string;        // Input filename
    UseStdIn: Boolean;        // Read from standard input
    ShowHelp: Boolean;        // Display help information
  end;

const
  BUFFER_SIZE = 4096; // 4KB buffer for efficient reading

var
  Config: TDumpConfig;
  InputFile: File;
  InputFileSize: Int64;
  IsUsingStdIn: Boolean;

{ Validate and adjust count value based on output mode }
function ValidateAndAdjustCount(Count: Integer; Mode: TOutputMode): Integer;
begin
  Result := Count;
  
  case Mode of
    omByte:
      begin
        // Byte mode: minimum count is 1
        if Result < 1 then
          Result := 1;
      end;
    omWord:
      begin
        // Word mode: minimum count is 2, must be even
        if Result < 2 then
          Result := 2
        else if (Result mod 2) <> 0 then
          Result := (Result div 2) * 2; // Round down to nearest even number
      end;
    omDWord:
      begin
        // DWord mode: minimum count is 4, must be multiple of 4
        if Result < 4 then
          Result := 4
        else if (Result mod 4) <> 0 then
          Result := (Result div 4) * 4; // Round down to nearest multiple of 4
      end;
  end;
end;

{ Initialize configuration with default values }
procedure InitializeConfig(var AConfig: TDumpConfig);
begin
  AConfig.Count := 16;              // Default 16 bytes per line
  AConfig.OutputMode := omByte;     // Default byte mode
  AConfig.Verbose := False;         // No verbose output by default
  AConfig.InputFile := '';          // No input file by default
  AConfig.UseStdIn := True;         // Use stdin by default
  AConfig.ShowHelp := False;        // Don't show help by default
end;

{ Display error message and usage information }
procedure ShowError(const ErrorMsg: string);
begin
  WriteLn('Error: ', ErrorMsg);
  WriteLn;
  WriteLn('Use -h or --help for usage information.');
end;

{ Display file access error and exit gracefully }
procedure ShowFileError(const FileName, ErrorMsg: string);
begin
  WriteLn('Error: Cannot access file "', FileName, '" - ', ErrorMsg);
  WriteLn('Please check that the file exists and you have permission to read it.');
end;

{ Parse command line arguments and populate configuration }
function ParseCommandLine(var AConfig: TDumpConfig): Boolean;
var
  i: Integer;
  Param: string;
  CountValue: Integer;
  Code: Integer;
begin
  Result := True;
  i := 1;
  
  while i <= ParamCount do
  begin
    Param := ParamStr(i);
    
    // Handle short and long form options
    if (Param = '-c') or (Param = '--count') then
    begin
      // Count option requires a parameter
      if i >= ParamCount then
      begin
        ShowError('Option ' + Param + ' requires a count value');
        Result := False;
        Exit;
      end;
      
      Inc(i);
      Val(ParamStr(i), CountValue, Code);
      if Code <> 0 then
      begin
        ShowError('Invalid count value "' + ParamStr(i) + '"');
        Result := False;
        Exit;
      end;
      
      if (CountValue < 1) or (CountValue > 1024) then
      begin
        ShowError('Count must be from 1 to 1024');
        Result := False;
        Exit;
      end;
      
      AConfig.Count := CountValue;
    end
    else if (Param = '-w') or (Param = '--word') then
    begin
      AConfig.OutputMode := omWord;
      // Re-validate count for new mode
      AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
    end
    else if (Param = '-d') or (Param = '--dword') then
    begin
      AConfig.OutputMode := omDWord;
      // Re-validate count for new mode
      AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
    end
    else if (Param = '-v') or (Param = '--verbose') then
    begin
      AConfig.Verbose := True;
    end
    else if (Param = '-h') or (Param = '--help') then
    begin
      AConfig.ShowHelp := True;
    end
    else if Param[1] = '-' then
    begin
      // Unknown option
      ShowError('Unknown option "' + Param + '"');
      Result := False;
      Exit;
    end
    else
    begin
      // This is the filename (last non-option argument)
      AConfig.InputFile := Param;
      AConfig.UseStdIn := False;
    end;
    
    Inc(i);
  end;
  
  // Validate and adjust count based on output mode
  AConfig.Count := ValidateAndAdjustCount(AConfig.Count, AConfig.OutputMode);
end;

{ Open input file or prepare standard input for reading }
function OpenInput(const FileName: string; UseStdIn: Boolean): Boolean;
var
  IOError: Integer;
begin
  Result := True;
  IsUsingStdIn := UseStdIn;
  
  if UseStdIn then
  begin
    // For standard input, we can't determine size in advance
    InputFileSize := -1;
    // Test if stdin is available
    try
      {$I-} // Turn off I/O checking
      if EOF(Input) then
      begin
        // This is not necessarily an error - stdin might just be empty
      end;
      {$I+} // Turn on I/O checking
      IOError := IOResult;
      if IOError <> 0 then
      begin
        ShowFileError('stdin', 'standard input not available');
        Result := False;
        Exit;
      end;
    except
      on E: Exception do
      begin
        ShowFileError('stdin', E.Message);
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    // Check if file exists
    if not FileExists(FileName) then
    begin
      ShowFileError(FileName, 'file not found');
      Result := False;
      Exit;
    end;
    
    // Try to open the file
    try
      AssignFile(InputFile, FileName);
      {$I-} // Turn off I/O checking
      Reset(InputFile, 1); // Open in binary mode (1 byte records)
      {$I+} // Turn on I/O checking
      
      IOError := IOResult;
      if IOError <> 0 then
      begin
        case IOError of
          2: ShowFileError(FileName, 'file not found');
          3: ShowFileError(FileName, 'path not found');
          5: ShowFileError(FileName, 'access denied - permission denied or file in use');
          32: ShowFileError(FileName, 'sharing violation - file is being used by another process');
        else
          ShowFileError(FileName, 'I/O error code ' + IntToStr(IOError));
        end;
        Result := False;
        Exit;
      end;
      
      // Get file size
      {$I-} // Turn off I/O checking
      InputFileSize := FileSize(InputFile);
      {$I+} // Turn on I/O checking
      
      IOError := IOResult;
      if IOError <> 0 then
      begin
        ShowFileError(FileName, 'cannot determine file size - I/O error code ' + IntToStr(IOError));
        CloseFile(InputFile);
        Result := False;
        Exit;
      end;
      
    except
      on E: Exception do
      begin
        ShowFileError(FileName, E.Message);
        Result := False;
        Exit;
      end;
    end;
  end;
end;

{ Get the size of the input file in bytes }
function GetFileSize(): Int64;
begin
  Result := InputFileSize;
end;

{ Close input file and cleanup resources }
procedure CloseInput();
begin
  if not IsUsingStdIn then
  begin
    try
      {$I-} // Turn off I/O checking
      CloseFile(InputFile);
      {$I+} // Turn on I/O checking
      IOResult; // Clear any pending I/O error
    except
      // Ignore errors during cleanup
    end;
  end;
end;

{ Read next chunk of data from input source }
function ReadNextChunk(var Buffer: array of Byte; MaxBytes: Integer): Integer;
var
  BytesRead: SmallInt;
  ActualMaxBytes: SmallInt;
  IOError: Integer;
  i: Integer;
  ch: Char;
begin
  Result := 0;
  BytesRead := 0;
  
  // Ensure we don't read more than buffer size or requested amount
  ActualMaxBytes := MaxBytes;
  if ActualMaxBytes > Length(Buffer) then
    ActualMaxBytes := Length(Buffer);
  if ActualMaxBytes > BUFFER_SIZE then
    ActualMaxBytes := BUFFER_SIZE;
    
  if IsUsingStdIn then
  begin
    // Read from standard input byte by byte
    try
      i := 0;
      while (i < ActualMaxBytes) and not EOF(Input) do
      begin
        {$I-} // Turn off I/O checking
        Read(Input, ch);
        {$I+} // Turn on I/O checking
        
        IOError := IOResult;
        if IOError <> 0 then
        begin
          if IOError <> 100 then // 100 is EOF, which is normal
          begin
            WriteLn('Error: Failed to read from standard input - I/O error code ', IOError);
          end;
          Break;
        end;
        
        Buffer[i] := Ord(ch);
        Inc(i);
      end;
      Result := i;
    except
      on E: Exception do
      begin
        WriteLn('Error: Exception while reading from standard input - ', E.Message);
        Result := 0; // Error occurred
      end;
    end;
  end
  else
  begin
    // Read from file
    try
      {$I-} // Turn off I/O checking
      BlockRead(InputFile, Buffer[0], ActualMaxBytes, BytesRead);
      {$I+} // Turn on I/O checking
      
      IOError := IOResult;
      if IOError = 0 then
        Result := BytesRead
      else
      begin
        if IOError <> 100 then // 100 is EOF, which is normal
        begin
          WriteLn('Error: Failed to read from file - I/O error code ', IOError);
        end;
        Result := 0; // Error or EOF
      end;
    except
      on E: Exception do
      begin
        WriteLn('Error: Exception while reading from file - ', E.Message);
        Result := 0; // Error occurred
      end;
    end;
  end;
end;

{ Calculate number of output lines based on file size and bytes per line }
function CalculateLineCount(FileSize: Int64; BytesPerLine: Integer): Integer;
begin
  if FileSize <= 0 then
    Result := 0
  else
    Result := (FileSize + BytesPerLine - 1) div BytesPerLine; // Ceiling division
end;

{ Format verbose header/footer line with file information }
function FormatVerboseLine(const FileName: string; FileSize: Int64; LineCount: Integer; IsHeader: Boolean): string;
var
  DateTime: TDateTime;
  DateTimeStr: string;
  SizeStr: string;
  LineCountStr: string;
  FileNameStr: string;
  Prefix: string;
begin
  // Get current date and time in ISO format
  DateTime := Now;
  DateTimeStr := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', DateTime);
  
  // Format file size
  if FileSize >= 0 then
    SizeStr := IntToStr(FileSize) + ' bytes'
  else
    SizeStr := 'unknown size';
  
  // Format line count
  LineCountStr := IntToStr(LineCount) + ' lines';
  
  // Handle filename display - different for file vs stdin
  if FileName = '' then
    FileNameStr := 'stdin'
  else
    FileNameStr := FileName;
  
  // Choose prefix based on header/footer
  if IsHeader then
    Prefix := 'Begin'
  else
    Prefix := 'End';
  
  // Format the complete verbose line
  Result := Format('%s dump of %s at %s (%s, %s)', 
    [Prefix, FileNameStr, DateTimeStr, SizeStr, LineCountStr]);
end;

{ Display verbose header for file input }
procedure ShowVerboseHeader(const FileName: string; FileSize: Int64; BytesPerLine: Integer);
var
  LineCount: Integer;
begin
  LineCount := CalculateLineCount(FileSize, BytesPerLine);
  WriteLn(FormatVerboseLine(FileName, FileSize, LineCount, True));
end;

{ Display verbose footer for both file and stdin input }
procedure ShowVerboseFooter(const FileName: string; FileSize: Int64; ActualLinesProcessed: Integer);
begin
  WriteLn(FormatVerboseLine(FileName, FileSize, ActualLinesProcessed, False));
end;

{ Format a single line of hexadecimal output }
function FormatHexLine(const Buffer: array of Byte; Count: Integer; Offset: Int64; Mode: TOutputMode; ExpectedBytesPerLine: Integer): string;
var
  i: Integer;
  HexPart, AsciiPart: string;
  ByteValue: Byte;
  AsciiChar: Char;
  GroupSize: Integer;
  ExpectedHexWidth: Integer;
begin
  // Initialize parts
  HexPart := '';
  AsciiPart := '';
  
  // Format offset as 8-digit hexadecimal
  Result := Format('%.8X  ', [Offset]);
  
  // Determine group size based on mode
  case Mode of
    omByte: GroupSize := 1;   // 1 byte per group (2 hex digits)
    omWord: GroupSize := 2;   // 2 bytes per group (4 hex digits)
    omDWord: GroupSize := 4;  // 4 bytes per group (8 hex digits)
  end;
  
  // Process each byte in the buffer
  for i := 0 to Count - 1 do
  begin
    ByteValue := Buffer[i];
    
    // Add hex representation (always 2 digits per byte)
    HexPart := HexPart + Format('%.2X', [ByteValue]);
    
    // Add space after complete groups (but not after the last byte)
    if (i < Count - 1) and ((i + 1) mod GroupSize = 0) then
      HexPart := HexPart + ' ';
    
    // Add ASCII representation
    AsciiChar := Chr(ByteValue);
    if (ByteValue >= 32) and (ByteValue <= 126) then
      AsciiPart := AsciiPart + AsciiChar  // Printable character (32-126)
    else
      AsciiPart := AsciiPart + '.';       // Non-printable character
  end;
  
  // Pad hex part to consistent width for alignment based on expected bytes per line
  // This ensures the ASCII part is always aligned, even for partial last lines
  case Mode of
    omByte:
      // Byte mode: 2 chars per byte + 1 space between each byte (except last)
      // For 16 bytes: "01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10" = 47 chars
      if ExpectedBytesPerLine > 1 then
        ExpectedHexWidth := ExpectedBytesPerLine * 3 - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
    omWord:
      // Word mode: 4 chars per word + 1 space between words (except last)
      // For 16 bytes (8 words): "0102 0304 0506 0708 090A 0B0C 0D0E 0F10" = 39 chars
      if ExpectedBytesPerLine > 2 then
        ExpectedHexWidth := ExpectedBytesPerLine * 2 + (ExpectedBytesPerLine div 2) - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
    omDWord:
      // DWord mode: 8 chars per dword + 1 space between dwords (except last)
      // For 16 bytes (4 dwords): "01020304 05060708 090A0B0C 0D0E0F10" = 35 chars
      if ExpectedBytesPerLine > 4 then
        ExpectedHexWidth := ExpectedBytesPerLine * 2 + (ExpectedBytesPerLine div 4) - 1
      else
        ExpectedHexWidth := ExpectedBytesPerLine * 2;
  end;
  
  // Pad to the expected width
  while Length(HexPart) < ExpectedHexWidth do
    HexPart := HexPart + ' ';
  
  // Combine all parts: offset + hex data + ASCII
  Result := Result + HexPart + '  ' + AsciiPart;
end;

{ Display help information and usage examples }
procedure ShowHelp;
begin
  WriteLn('tdump - Hexadecimal dump utility');
  WriteLn;
  WriteLn('Usage: tdump [options] [filename]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -c, --count N     Number of bytes to display per line, from 1 to 1024 (default: 16)');
  WriteLn('  -w, --word        Group output as 16-bit words (4 hex digits per group)');
  WriteLn('  -d, --dword       Group output as 32-bit dwords (8 hex digits per group)');
  WriteLn('  -v, --verbose     Display file information header and footer');
  WriteLn('  -h, --help        Show this help information');
  WriteLn;
  WriteLn('Examples:');
  WriteLn('  tdump file.bin              # Dump file.bin with default settings');
  WriteLn('  tdump -c 8 file.bin         # Display 8 bytes per line');
  WriteLn('  tdump -w -c 16 file.bin     # Word mode with 16 bytes per line');
  WriteLn('  tdump -v file.bin           # Verbose output with file information');
  WriteLn('  cat file.bin | tdump        # Read from standard input');
  WriteLn('  tdump < file.bin            # Read from standard input (alternative)');
  WriteLn;
  WriteLn('If no filename is provided, tdump reads from standard input.');
  WriteLn('Output format: OFFSET  HEX_DATA  ASCII_REPRESENTATION');
end;

{ Main program entry point }
var
  Buffer: array[0..BUFFER_SIZE-1] of Byte;
  BytesRead: Integer;
  CurrentOffset: Int64;
  LinesOutputted: Integer;

begin
  // Initialize configuration with defaults
  InitializeConfig(Config);
  
  // Parse command line arguments
  if not ParseCommandLine(Config) then
  begin
    Halt(1); // Exit with error code
  end;
  
  // Handle help option
  if Config.ShowHelp then
  begin
    ShowHelp;
    Halt(0); // Exit successfully
  end;
  
  // Open input source
  if not OpenInput(Config.InputFile, Config.UseStdIn) then
  begin
    Halt(1); // Exit with error code
  end;
    
  try
    try
      // Initialize counters
      CurrentOffset := 0;
      LinesOutputted := 0;
      
      // Display verbose header for file input (not for stdin)
      if Config.Verbose and not Config.UseStdIn then
      begin
        ShowVerboseHeader(Config.InputFile, GetFileSize(), Config.Count);
      end;
      
      // Main processing loop - read data chunks and format output
      repeat
        BytesRead := ReadNextChunk(Buffer, Config.Count);
        if BytesRead > 0 then
        begin
          // Format and output the hex line
          WriteLn(FormatHexLine(Buffer, BytesRead, CurrentOffset, Config.OutputMode, Config.Count));
          
          // Update offset and line count for accurate position tracking
          CurrentOffset := CurrentOffset + BytesRead;
          Inc(LinesOutputted);
        end;
      until BytesRead = 0;
      
      // Display verbose footer for both file and stdin input
      if Config.Verbose then
      begin
        if Config.UseStdIn then
          // For stdin, use actual processed data (CurrentOffset = total bytes processed)
          ShowVerboseFooter('', CurrentOffset, LinesOutputted)
        else
          // For files, use actual lines outputted for accuracy
          ShowVerboseFooter(Config.InputFile, GetFileSize(), LinesOutputted);
      end;
      
    except
      on E: EInOutError do
      begin
        WriteLn('Error: I/O operation failed - ', E.Message);
        WriteLn('Processing stopped at offset ', Format('%.8X', [CurrentOffset]));
        Halt(1);
      end;
      on E: EAccessViolation do
      begin
        WriteLn('Error: Memory access violation - ', E.Message);
        WriteLn('Processing stopped at offset ', Format('%.8X', [CurrentOffset]));
        Halt(1);
      end;
      on E: Exception do
      begin
        WriteLn('Error: Unexpected error during processing - ', E.Message);
        WriteLn('Processing stopped at offset ', Format('%.8X', [CurrentOffset]));
        Halt(1);
      end;
    end;
    
  finally
    // Always cleanup resources, even in error scenarios
    CloseInput();
  end;
end.