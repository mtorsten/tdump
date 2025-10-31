program test_integration;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Process;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

{ Test helper procedures }
procedure AssertEqual(Expected, Actual: string; const TestName: string);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName);
    WriteLn('  Expected: "', Expected, '"');
    WriteLn('  Actual:   "', Actual, '"');
    Inc(TestsFailed);
  end;
end;

procedure AssertContains(const Substring, Text: string; const TestName: string);
begin
  if Pos(Substring, Text) > 0 then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName);
    WriteLn('  Expected substring: "', Substring, '"');
    WriteLn('  In text: "', Text, '"');
    Inc(TestsFailed);
  end;
end;

procedure AssertExitCode(Expected, Actual: Integer; const TestName: string);
begin
  if Expected = Actual then
  begin
    WriteLn('PASS: ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('FAIL: ', TestName, ' - Expected exit code: ', Expected, ', Actual: ', Actual);
    Inc(TestsFailed);
  end;
end;

{ Execute tdump with given arguments and return output and exit code }
function RunTdump(const Args: string; const InputData: string; out Output: string; out ExitCode: Integer): Boolean;
var
  Process: TProcess;
  InputStream: TStringStream;
  OutputStream: TStringStream;
  BytesRead: Integer;
  Buffer: array[0..1023] of Char;
begin
  Result := False;
  Output := '';
  ExitCode := -1;
  
  Process := TProcess.Create(nil);
  InputStream := nil;
  OutputStream := TStringStream.Create('');
  
  try
    Process.Executable := './tdump';
    Process.Parameters.DelimitedText := Args;
    Process.Options := [poUsePipes, poWaitOnExit];
    
    // Set up input if provided
    if InputData <> '' then
    begin
      InputStream := TStringStream.Create(InputData);
      Process.Input := InputStream;
    end;
    
    Process.Output := OutputStream;
    Process.Execute;
    
    // Read output
    repeat
      BytesRead := Process.Output.Read(Buffer[0], SizeOf(Buffer));
      if BytesRead > 0 then
        Output := Output + Copy(Buffer, 1, BytesRead);
    until BytesRead = 0;
    
    ExitCode := Process.ExitStatus;
    Result := True;
    
  except
    on E: Exception do
    begin
      WriteLn('Error running tdump: ', E.Message);
      Result := False;
    end;
  end;
  
  if Assigned(InputStream) then
    InputStream.Free;
  OutputStream.Free;
  Process.Free;
end;

{ Create a test binary file with known content }
procedure CreateTestFile(const FileName: string; const Content: array of Byte);
var
  F: File;
  i: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F, 1);
  try
    for i := 0 to High(Content) do
      BlockWrite(F, Content[i], 1);
  finally
    CloseFile(F);
  end;
end;

{ Test procedures }

procedure TestBasicFileOutput;
var
  TestData: array[0..15] of Byte;
  Output: string;
  ExitCode: Integer;
  i: Integer;
begin
  WriteLn('=== Testing Basic File Output ===');
  
  // Create test file with known content
  for i := 0 to 15 do
    TestData[i] := i + 1;
  CreateTestFile('test_basic.bin', TestData);
  
  try
    // Test basic file dump
    if RunTdump('test_basic.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Basic file dump should exit successfully');
      AssertContains('00000000  01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F 10', Output, 'Should contain correct hex output');
      AssertContains('................', Output, 'Should contain ASCII representation');
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_basic.bin');
  end;
end;

procedure TestDifferentModes;
var
  TestData: array[0..7] of Byte;
  Output: string;
  ExitCode: Integer;
  i: Integer;
begin
  WriteLn('=== Testing Different Output Modes ===');
  
  // Create test file with 8 bytes
  for i := 0 to 7 do
    TestData[i] := i + 1;
  CreateTestFile('test_modes.bin', TestData);
  
  try
    // Test word mode
    if RunTdump('-w test_modes.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Word mode should exit successfully');
      AssertContains('0102 0304 0506 0708', Output, 'Word mode should group bytes correctly');
    end
    else
      Inc(TestsFailed);
    
    // Test dword mode
    if RunTdump('-d test_modes.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'DWord mode should exit successfully');
      AssertContains('01020304 05060708', Output, 'DWord mode should group bytes correctly');
    end
    else
      Inc(TestsFailed);
      
    // Test custom count
    if RunTdump('-c 4 test_modes.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Custom count should exit successfully');
      AssertContains('00000000  01 02 03 04', Output, 'Should show first 4 bytes on first line');
      AssertContains('00000004  05 06 07 08', Output, 'Should show next 4 bytes on second line');
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_modes.bin');
  end;
end;

procedure TestVerboseOutput;
var
  TestData: array[0..3] of Byte;
  Output: string;
  ExitCode: Integer;
  i: Integer;
begin
  WriteLn('=== Testing Verbose Output ===');
  
  // Create small test file
  for i := 0 to 3 do
    TestData[i] := i + 65; // 'A', 'B', 'C', 'D'
  CreateTestFile('test_verbose.bin', TestData);
  
  try
    // Test verbose mode
    if RunTdump('-v test_verbose.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Verbose mode should exit successfully');
      AssertContains('Begin dump of test_verbose.bin', Output, 'Should contain verbose header');
      AssertContains('End dump of test_verbose.bin', Output, 'Should contain verbose footer');
      AssertContains('4 bytes', Output, 'Should show correct file size');
      AssertContains('1 lines', Output, 'Should show correct line count');
      AssertContains('00000000  41 42 43 44', Output, 'Should contain hex data');
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_verbose.bin');
  end;
end;

procedure TestStandardInput;
var
  Output: string;
  ExitCode: Integer;
  InputData: string;
begin
  WriteLn('=== Testing Standard Input Processing ===');
  
  // Test with simple input
  InputData := 'Hello';
  if RunTdump('', InputData, Output, ExitCode) then
  begin
    AssertExitCode(0, ExitCode, 'Stdin processing should exit successfully');
    AssertContains('00000000  48 65 6C 6C 6F', Output, 'Should contain correct hex for "Hello"');
    AssertContains('Hello', Output, 'Should contain ASCII representation');
  end
  else
    Inc(TestsFailed);
    
  // Test verbose with stdin
  if RunTdump('-v', InputData, Output, ExitCode) then
  begin
    AssertExitCode(0, ExitCode, 'Verbose stdin should exit successfully');
    AssertContains('End dump of stdin', Output, 'Should contain verbose footer for stdin');
    AssertContains('5 bytes', Output, 'Should show correct byte count');
  end
  else
    Inc(TestsFailed);
end;

procedure TestErrorScenarios;
var
  Output: string;
  ExitCode: Integer;
begin
  WriteLn('=== Testing Error Scenarios ===');
  
  // Test non-existent file
  if RunTdump('nonexistent.bin', '', Output, ExitCode) then
  begin
    AssertExitCode(1, ExitCode, 'Non-existent file should exit with error');
    AssertContains('Error:', Output, 'Should contain error message');
    AssertContains('nonexistent.bin', Output, 'Should mention the filename');
  end
  else
    Inc(TestsFailed);
    
  // Test invalid option
  if RunTdump('-x', '', Output, ExitCode) then
  begin
    AssertExitCode(1, ExitCode, 'Invalid option should exit with error');
    AssertContains('Error:', Output, 'Should contain error message');
    AssertContains('Unknown option', Output, 'Should mention unknown option');
  end
  else
    Inc(TestsFailed);
    
  // Test invalid count
  if RunTdump('-c abc', '', Output, ExitCode) then
  begin
    AssertExitCode(1, ExitCode, 'Invalid count should exit with error');
    AssertContains('Error:', Output, 'Should contain error message');
    AssertContains('Invalid count', Output, 'Should mention invalid count');
  end
  else
    Inc(TestsFailed);
    
  // Test count without value
  if RunTdump('-c', '', Output, ExitCode) then
  begin
    AssertExitCode(1, ExitCode, 'Count without value should exit with error');
    AssertContains('Error:', Output, 'Should contain error message');
  end
  else
    Inc(TestsFailed);
end;

procedure TestHelpOption;
var
  Output: string;
  ExitCode: Integer;
begin
  WriteLn('=== Testing Help Option ===');
  
  // Test short help option
  if RunTdump('-h', '', Output, ExitCode) then
  begin
    AssertExitCode(0, ExitCode, 'Help option should exit successfully');
    AssertContains('tdump - Hexadecimal dump utility', Output, 'Should contain program description');
    AssertContains('Usage:', Output, 'Should contain usage information');
    AssertContains('Options:', Output, 'Should contain options list');
    AssertContains('Examples:', Output, 'Should contain examples');
  end
  else
    Inc(TestsFailed);
    
  // Test long help option
  if RunTdump('--help', '', Output, ExitCode) then
  begin
    AssertExitCode(0, ExitCode, 'Long help option should exit successfully');
    AssertContains('tdump - Hexadecimal dump utility', Output, 'Should contain program description');
  end
  else
    Inc(TestsFailed);
end;

procedure TestEdgeCases;
var
  TestData: array[0..0] of Byte;
  Output: string;
  ExitCode: Integer;
begin
  WriteLn('=== Testing Edge Cases ===');
  
  // Test empty file
  CreateTestFile('test_empty.bin', TestData[0..0]);
  try
    DeleteFile('test_empty.bin'); // Create and immediately delete to get empty file
    AssignFile(TestData, 'test_empty.bin');
    Rewrite(TestData);
    CloseFile(TestData);
    
    if RunTdump('test_empty.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Empty file should exit successfully');
      // Empty file should produce no hex output, just verbose info if enabled
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_empty.bin');
  end;
  
  // Test single byte file
  TestData[0] := 65; // 'A'
  CreateTestFile('test_single.bin', TestData[0..0]);
  try
    if RunTdump('test_single.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Single byte file should exit successfully');
      AssertContains('00000000  41', Output, 'Should contain single byte hex');
      AssertContains('A', Output, 'Should contain ASCII representation');
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_single.bin');
  end;
end;

procedure TestLongOptions;
var
  TestData: array[0..7] of Byte;
  Output: string;
  ExitCode: Integer;
  i: Integer;
begin
  WriteLn('=== Testing Long Options ===');
  
  // Create test file
  for i := 0 to 7 do
    TestData[i] := i + 1;
  CreateTestFile('test_long.bin', TestData);
  
  try
    // Test long form options
    if RunTdump('--count 4 --word --verbose test_long.bin', '', Output, ExitCode) then
    begin
      AssertExitCode(0, ExitCode, 'Long options should exit successfully');
      AssertContains('Begin dump of test_long.bin', Output, 'Should contain verbose header');
      AssertContains('0102 0304', Output, 'Should use word mode');
      AssertContains('0506 0708', Output, 'Should show second line with remaining bytes');
    end
    else
      Inc(TestsFailed);
      
  finally
    DeleteFile('test_long.bin');
  end;
end;

{ Main test execution }
begin
  WriteLn('Running tdump integration tests...');
  WriteLn;
  
  // Check if tdump executable exists
  if not FileExists('./tdump') then
  begin
    WriteLn('Error: tdump executable not found. Please compile tdump first.');
    Halt(1);
  end;
  
  TestsPassed := 0;
  TestsFailed := 0;
  
  TestBasicFileOutput;
  WriteLn;
  
  TestDifferentModes;
  WriteLn;
  
  TestVerboseOutput;
  WriteLn;
  
  TestStandardInput;
  WriteLn;
  
  TestErrorScenarios;
  WriteLn;
  
  TestHelpOption;
  WriteLn;
  
  TestEdgeCases;
  WriteLn;
  
  TestLongOptions;
  WriteLn;
  
  WriteLn('=== Integration Test Results ===');
  WriteLn('Tests passed: ', TestsPassed);
  WriteLn('Tests failed: ', TestsFailed);
  WriteLn('Total tests: ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then
  begin
    WriteLn('All integration tests passed!');
    Halt(0);
  end
  else
  begin
    WriteLn('Some integration tests failed!');
    Halt(1);
  end;
end.