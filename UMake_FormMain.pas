unit UMake_FormMain;


interface


uses
  UMake_Configuration, UMake_Options,
  Windows, Messages, SysUtils, SysTools, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, FileCtrl, RegExpr, Math;


(*****************************************************************************)
(*  TInfoError
(*****************************************************************************)

type
  TInfoError = class
  private
    FTextMessageFormatted: string;
    FTextExplanation:      string;

    function GetTextMessageFormatted: string;
    function GetTextExplanation:      string;

  public
    IndexLine:   Integer;
    TextFile:    string;
    TextMessage: string;

    property TextMessageFormatted: string read GetTextMessageFormatted;
    property TextExplanation:      string read GetTextExplanation;
  end;


(*****************************************************************************)
(*  TFormMain
(*****************************************************************************)

type
  TFormMain = class(TForm)
    ButtonAbort: TButton;
    ButtonDetails: TButton;
    ButtonErrorEdit: TButton;
    ButtonOptions: TButton;
    ButtonWarningEdit: TButton;
    ButtonWarningNext: TButton;
    ButtonWarningPrev: TButton;
    ImageError: TImage;
    ImageWarning: TImage;
    LabelErrorLocation: TLabel;
    LabelErrorTitle: TLabel;
    LabelWarningLocation: TLabel;
    LabelWarningNumber: TLabel;
    LabelWarningTitle: TLabel;
    PageControlDetails: TPageControl;
    ProgressBar: TProgressBar;
    RichEditError: TRichEdit;
    RichEditMessages: TRichEdit;
    RichEditWarning: TRichEdit;
    StaticTextProgress: TStaticText;
    TabSheetError: TTabSheet;
    TabSheetMessages: TTabSheet;
    TabSheetWarnings: TTabSheet;

    procedure FormShow(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonDetailsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ButtonErrorEditClick(Sender: TObject);
    procedure ButtonWarningPrevClick(Sender: TObject);
    procedure ButtonWarningNextClick(Sender: TObject);
    procedure TabSheetWarningsResize(Sender: TObject);
    procedure TabSheetErrorResize(Sender: TObject);

  private
    Configuration: TConfiguration;
    Options: TOptions;

    IndexWarning: Integer;
    InfoError: TInfoError;
    ListInfoWarning: TList;

    FlagClosing: Boolean;
    PipedProcess: TPipedProcess;
    RegExprClass: TRegExpr;
    RegExprCompiling: TRegExpr;
    RegExprCompleted: TRegExpr;
    RegExprPackage: TRegExpr;
    RegExprParsing: TRegExpr;
    RegExprErrorCompile: TRegExpr;
    RegExprErrorParse: TRegExpr;
    RegExprWarningCompile: TRegExpr;
    RegExprWarningParse: TRegExpr;
    TextBufferPipe: string;
    TextFilePackageBackup: string;
    TextFilePackageOriginal: string;

    procedure ErrorMessageBox(TextMessage: string; OptionsMessage: Integer = MB_ICONERROR);
    procedure ErrorDetails(InfoError: TInfoError; LabelLocation: TLabel; RichEdit: TRichEdit);
    procedure PipedProcessDebug(Sender: TObject; const DebugEvent: TDebugEvent; var ContinueStatus: Cardinal);
    procedure PipedProcessOutput(Sender: TObject; const TextData: string; Pipe: TPipedOutput);
    procedure PipedProcessTerminate(Sender: TObject);
    procedure UpdateDetailsError;
    procedure UpdateDetailsWarning;
    procedure RichEditMessagesAppend(TextAppend: string; ColorAppend: TColor);

  public
    procedure Startup;
  end;


var
  FormMain: TFormMain;


implementation


uses
  UMake_FormOptions, UMake_FormLaunch;


const
  CRLF = #13#10;


{$R *.DFM}


(*****************************************************************************)
(*  Global
(*****************************************************************************)

function JoinArray(TextSeparator: string; ArrayStrings: array of string): string;
var
  IndexString: Integer;
begin
  Result := '';

  for IndexString := 0 to High(ArrayStrings) do
  begin
    AppendStr(Result, ArrayStrings[IndexString]);
    if IndexString < High(ArrayStrings) then
      AppendStr(Result, TextSeparator);
  end;
end;


(*****************************************************************************)
(*  TInfoError
(*****************************************************************************)

function TInfoError.GetTextMessageFormatted: string;
begin
  if Length(FTextMessageFormatted) = 0 then
  begin
    FTextMessageFormatted := TextMessage;

    FTextMessageFormatted := ReplaceRegExpr('\s+',           FTextMessageFormatted, ' ');
    FTextMessageFormatted := ReplaceRegExpr('^''|''$',       FTextMessageFormatted, '');
    FTextMessageFormatted := ReplaceRegExpr('(\W)''|''(\W)', FTextMessageFormatted, '$1$2', True);
    FTextMessageFormatted := ReplaceRegExpr('([^.])$',       FTextMessageFormatted, '$1.',  True);
  end;

  Result := FTextMessageFormatted;
end;


var
  StringListExplanations: TStringList = nil;


function TInfoError.GetTextExplanation: string;
var
  IndexCharSeparator: Integer;
  IndexExplanation: Integer;
  RegExprExplanation: TRegExpr;
  TextLineExplanation: string;
  TextFileExplanations: string;
begin
  if Length(FTextExplanation) = 0 then
  begin
    if not Assigned(StringListExplanations) then
    begin
      TextFileExplanations := ChangeFileExt(ParamStr(0), 'Explanations.txt');

      StringListExplanations := TStringList.Create;
      if FileExists(TextFileExplanations) then
        StringListExplanations.LoadFromFile(TextFileExplanations);
    end;

    RegExprExplanation := TRegExpr.Create;

    for IndexExplanation := 0 to StringListExplanations.Count - 1 do
    begin
      TextLineExplanation := StringListExplanations[IndexExplanation];

      IndexCharSeparator := Pos(#9, TextLineExplanation);
      if IndexCharSeparator = 0 then
        Continue;

      try
        RegExprExplanation.Expression := Copy(TextLineExplanation, 1, IndexCharSeparator - 1);

        if RegExprExplanation.Exec(TextMessageFormatted) then
        begin
          FTextExplanation := Copy(TextLineExplanation, IndexCharSeparator + 1, Length(TextLineExplanation));
          FTextExplanation := ReplaceRegExpr('\\n', FTextExplanation, CRLF);
          FTextExplanation := RegExprExplanation.Substitute(FTextExplanation);
          Break;
        end;
      except
        on ERegExpr do;
      end;
    end;

    RegExprExplanation.Free;

    if Length(FTextExplanation) = 0 then
      FTextExplanation := 'Sorry, no further explanation available.';
  end;

  Result := FTextExplanation;
end;


(*****************************************************************************)
(*  TFormMain
(*****************************************************************************)

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Options := TOptions.Create;

  ImageError  .Picture.Icon.Handle := LoadIcon(0, IDI_HAND);
  ImageWarning.Picture.Icon.Handle := LoadIcon(0, IDI_EXCLAMATION);

  PageControlDetails.ActivePage := TabSheetMessages;
  TabSheetError   .TabVisible := False;
  TabSheetWarnings.TabVisible := False;

  StaticTextProgress.DoubleBuffered := True;
  ProgressBar       .DoubleBuffered := True;
  PageControlDetails.DoubleBuffered := True;

  Constraints.MaxHeight := Constraints.MinHeight;
end;


procedure TFormMain.Startup;
var
  CountFiles: Integer;
  DateTimePackage: TDateTime;
  DateTimeSource: TDateTime;
  FlagSetup: Boolean;
  FlagUpdated: Boolean;
  IndexPackage: Integer;
  IndexParam: Integer;
  ResultFind: Integer;
  SearchRec: TSearchRec;
  TextDirPackage: string;
  TextFilePackage: string;
  TextFileSource: string;
  TextPackage: string;
begin
  FlagSetup := False;
  TextFileSource := '';

  for IndexParam := 1 to ParamCount do
  begin
    if AnsiSameText(ParamStr(IndexParam), '/setup') then
    begin
      FlagSetup := True;
    end

    else if (Length(ParamStr(IndexParam)) > 0) and (ParamStr(IndexParam)[1] <> '/') then
    begin
      if Length(TextFileSource) = 0 then
      begin
        TextFileSource := GetLongPath(ParamStr(IndexParam));
        if DirectoryExists(TextFileSource) then
          TextFileSource := IncludeTrailingBackslash(TextFileSource);
      end;
    end;
  end;

  if Length(TextFileSource) = 0 then
  begin
    if FlagSetup then
    begin
      FormOptions.Options := Options;
      FormOptions.ShowModal;
      Close;
    end
    else begin
      FormLaunch.Options := Options;
      if FormLaunch.ShowModal = mrOk
        then Configuration := FormLaunch.Configuration
        else Close;
    end;
  end
  else begin
    TextDirPackage := ExcludeTrailingBackslash(ExtractFilePath(TextFileSource));
    if AnsiSameText(ExtractFileName(TextDirPackage), 'Classes') then
      TextDirPackage := ExcludeTrailingBackslash(ExtractFilePath(TextDirPackage));

    try
      Configuration := TConfiguration.Create(ExtractFileName(TextDirPackage), ExtractFilePath(TextDirPackage));
    except
      on EConfigurationGameDirNotFound    do ErrorMessageBox('Game directory not found for the given file.');
      on EConfigurationGameDirInvalid     do ErrorMessageBox('UnrealScript project directories must be located directly below the game base directory.');
      on EConfigurationPackageDirNotFound do ErrorMessageBox('Package directory not found for the given file.');
      on EConfigurationPackageDirInvalid  do ErrorMessageBox('UnrealScript project directories must contain a "Classes" subdirectory for UnrealScript source files.');
    end;
  end;

  if Assigned(Configuration) then
  begin
    try
      Configuration.Read;
    except
      on EConfigurationGameIniNotFound do
      begin
        ErrorMessageBox('Unable to find the main game configuration file.');
        Close;
        Exit;
      end;
    end;

    if FlagSetup then
    begin
      FormOptions.Configuration := Configuration;
      FormOptions.Options := Options;
      FormOptions.ShowModal;
      Close;
    end
    else begin
      if not FileExists(IncludeTrailingBackslash(Configuration.DirGame) + Configuration.Package + '\make.ini') then
      begin
        FormOptions.Configuration := Configuration;
        FormOptions.Options := Options;

        if FormOptions.ShowModal <> mrOk then
        begin
          Close;
          Exit;
        end;
      end;

      FlagUpdated := False;

      for IndexPackage := 0 to Configuration.StringListPackages.Count - 1 do
      begin
        TextPackage := Configuration.StringListPackages[IndexPackage];
        TextFilePackage := Configuration.FindFilePackage(TextPackage);

        if AnsiSameText(TextPackage, Configuration.Package) or (Length(TextFilePackage) = 0) then
        begin
          CountFiles := 0;

          try
            if FileExists(TextFilePackage)
              then DateTimePackage := FileDateToDateTime(FileAge(TextFilePackage))
              else DateTimePackage := 0.0;

            ResultFind := FindFirst(IncludeTrailingBackslash(Configuration.DirGame) + TextPackage + '\Classes\*.uc', faAnyFile, SearchRec);
            while ResultFind = 0 do
            begin
              DateTimeSource := FileDateToDateTime(SearchRec.Time);
              if DateTimeSource > DateTimePackage then
                FlagUpdated := True;

              Inc(CountFiles);
              ResultFind := FindNext(SearchRec);
            end;
            FindClose(SearchRec);
          except
            on EInOutError do;
          end;

          if CountFiles = 0 then
          begin
            ErrorMessageBox(Format('No valid project directory found for package %s (which requires recompilation).', [TextPackage]));
            Close;
            Exit;
          end;

          ProgressBar.Max := ProgressBar.Max + CountFiles * 2;
        end
        else begin
          if not AnsiSameText(ExtractFileExt(TextFilePackage), '.u') then
          begin
            ErrorMessageBox(Format('Invalid dependency on non-code package %s.', [ExtractFileName(TextFilePackage)]));
            Close;
            Exit;
          end;

          ProgressBar.Max := ProgressBar.Max + 1;
        end;
      end;

      if FlagUpdated or (MessageBox(Application.Handle, 'Your project seems to be up to date. Compile anyway?', PChar(Application.Title), MB_ICONINFORMATION + MB_YESNO) = IDYES)
        then Show
        else Close;
    end;
  end
  else begin
    Close;
  end;
end;


procedure TFormMain.ErrorMessageBox(TextMessage: string; OptionsMessage: Integer);
begin
  Application.MessageBox(PChar(TextMessage), PChar(Application.Title), OptionsMessage);
end;


procedure TFormMain.ErrorDetails(InfoError: TInfoError; LabelLocation: TLabel; RichEdit: TRichEdit);
var
  IndexParagraph: Integer;
  RegExprParagraph: TRegExpr;
  StringListParagraphs: TStringList;
begin
  if Length(InfoError.TextFile) = 0 then
    LabelLocation.Caption := 'Occurred before compilation'
  else if InfoError.IndexLine = 0 then
    LabelLocation.Caption := ExtractFileName(InfoError.TextFile)
  else
    LabelLocation.Caption := Format('%s (line %d)', [ExtractFileName(InfoError.TextFile), InfoError.IndexLine]);

  RichEdit.Lines.BeginUpdate;
  RichEdit.Clear;

  RichEdit.SelAttributes.Size := RichEditError.Font.Size;
  RichEdit.SelAttributes.Style := [];
  RichEdit.Paragraph.FirstIndent := 2;

  RichEdit.SelText := InfoError.TextMessageFormatted + CRLF;

  if Length(InfoError.TextExplanation) > 0 then
  begin
    RichEdit.SelAttributes.Size := 6;
    RichEdit.SelText := CRLF;

    RichEdit.SelAttributes.Size := RichEditError.Font.Size;
    RichEdit.SelAttributes.Style := [fsBold];
    RichEdit.SelText := 'Explanation' + CRLF;
    RichEdit.SelAttributes.Style := [];

    StringListParagraphs := TStringList.Create;

    RegExprParagraph := TRegExpr.Create;
    RegExprParagraph.Expression := '(\r?\n)+';
    RegExprParagraph.Split(InfoError.TextExplanation, StringListParagraphs);

    for IndexParagraph := 0 to StringListParagraphs.Count - 1 do
    begin
      RichEdit.SelAttributes.Size := 3;
      RichEdit.SelText := CRLF;

      RichEdit.SelAttributes.Size := RichEditError.Font.Size;
      RichEdit.SelText := StringListParagraphs[IndexParagraph];

      if IndexParagraph < StringListParagraphs.Count - 1 then
        RichEdit.SelText := CRLF;
    end;

    RichEdit.Perform(WM_VSCROLL, SB_TOP, 0);

    StringListParagraphs.Free;
    RegExprParagraph.Free;
  end;

  RichEditError.Lines.EndUpdate;

  if ButtonDetails.Enabled then
    ButtonDetailsClick(ButtonDetails);
end;


procedure TFormMain.FormShow(Sender: TObject);
var
  TextCommand: string;
  TextDirSystem: string;
begin
  TextFilePackageOriginal := Configuration.FindFilePackage(Configuration.Package);

  if Length(TextFilePackageOriginal) > 0 then
  begin
    TextFilePackageBackup := TextFilePackageOriginal + '.backup';

    if FileExists(TextFilePackageBackup) then
      DeleteFile(TextFilePackageBackup);

    while not RenameFile(TextFilePackageOriginal, TextFilePackageBackup) do
    begin
      if Application.MessageBox(PChar(Format('UMake is unable to rename %s before recompiling it. Please make sure that the file isn''t loaded in UnrealEd or any other application at the moment.', [ExtractFileName(TextFilePackageOriginal)])), PChar(Application.Title), MB_ICONERROR + MB_RETRYCANCEL) <> IDRETRY then
      begin
        Close;
        Exit;
      end;
    end;
  end;

  RichEditMessages.Paragraph.FirstIndent := 2;
  RichEditMessages.Paragraph.LeftIndent  := 6;

  RegExprClass          := TRegExpr.Create;
  RegExprPackage        := TRegExpr.Create;
  RegExprParsing        := TRegExpr.Create;
  RegExprCompiling      := TRegExpr.Create;
  RegExprCompleted      := TRegExpr.Create;
  RegExprErrorCompile   := TRegExpr.Create;
  RegExprErrorParse     := TRegExpr.Create;
  RegExprWarningCompile := TRegExpr.Create;
  RegExprWarningParse   := TRegExpr.Create;

  RegExprClass         .Expression := '^([^.]+\.)?(\w+)';
  RegExprPackage       .Expression := '^-+\s*(\w+)(\s*-\s*(\w+))?';
  RegExprParsing       .Expression := '^Parsing\s+(\w+)';
  RegExprCompiling     .Expression := '^Compiling\s+(\w+)';
  RegExprCompleted     .Expression := '^(Success|Failure) - \d+ error\(s\)';
  RegExprErrorCompile  .Expression := '^([A-Za-z]:\\.*?\\Classes\\\w+\.uc)\s*\((\d+)\)\s*:\s*Error,\s*(.*)';
  RegExprErrorParse    .Expression := '^Script vs. class name mismatch \((([^/]+))/[^)]+\)|^Bad class definition|^Superclass \S+ of class ((\S+)) not found|^([^:]+: )Unknown property|^ObjectProperty ([^.]+\.[^.]+\.)';
  RegExprWarningCompile.Expression := '^([A-Za-z]:\\.*?\\Classes\\\w+\.uc)\s*\((\d+)\)\s*:\s*ExecWarning,\s*(.*)';
  RegExprWarningParse  .Expression := '^Failed loading\s+.*';

  InfoError := TInfoError.Create;
  ListInfoWarning := TList.Create;

  Configuration.Write;
  TextDirSystem := IncludeTrailingBackslash(Configuration.DirGame) + 'System';

  TextCommand := Format('%s make ini=%s',
    [GetQuotedParam(IncludeTrailingBackslash(TextDirSystem) + 'ucc.exe'),
     GetQuotedParam(GetRelativePath(IncludeTrailingBackslash(Configuration.DirPackage) + 'make.ini', IncludeTrailingBackslash(TextDirSystem)))]);

  PipedProcess := TPipedProcess.Create;
  PipedProcess.Directory   := TextDirSystem;
  PipedProcess.Command     := TextCommand;
  PipedProcess.OnDebug     := PipedProcessDebug;
  PipedProcess.OnOutput    := PipedProcessOutput;
  PipedProcess.OnTerminate := PipedProcessTerminate;
  PipedProcess.Debug;

  StaticTextProgress.SetFocus;
end;


procedure TFormMain.PipedProcessDebug(Sender: TObject; const DebugEvent: TDebugEvent; var ContinueStatus: Cardinal);
begin
  // nothing
end;


procedure TFormMain.PipedProcessOutput(Sender: TObject; const TextData: string; Pipe: TPipedOutput);
var
  ColorLine: TColor;
  IndexCharSeparator: Integer;
  IndexMatch: Integer;
  InfoWarning: TInfoError;
  TextLine: string;


  function FormatError(TextType: string; InfoError: TInfoError): string;
  begin
    Result := Format('%s in %s (%d): %s', [TextType, ExtractFileName(InfoError.TextFile),
                                                                     InfoError.IndexLine,
                                                                     InfoError.TextMessage]);
  end;


begin
  if not FlagClosing then
    ButtonAbort.Enabled := True;

  RichEditMessages.Lines.BeginUpdate;

  AppendStr(TextBufferPipe, TextData);
  while Length(TextBufferPipe) > 0 do
  begin
    IndexCharSeparator := Pos(#10, TextBufferPipe);
    if IndexCharSeparator = 0 then
      Break;

    TextLine := TrimRight(Copy(TextBufferPipe, 1, IndexCharSeparator));
    Delete(TextBufferPipe, 1, IndexCharSeparator);

    ColorLine := RichEditMessages.Font.Color;

    if RegExprPackage.Exec(TextLine) then
    begin
      TextLine := Format('----- %s', [RegExprPackage.Match[1]]);
      if RegExprPackage.SubExprMatchCount > 1 then
        AppendStr(TextLine, Format(' (%s)', [RegExprPackage.Match[3]]));

      if not FlagClosing then
      begin
        if ProgressBar.Position < ProgressBar.Max then
          ProgressBar.StepIt;
        StaticTextProgress.Caption := Format('Reading %s', [RegExprPackage.Match[1]]);
      end;
    end

    else if RegExprParsing.Exec(TextLine) then
    begin
      if not FlagClosing then
      begin
        if ProgressBar.Position < ProgressBar.Max then
          ProgressBar.StepIt;
        StaticTextProgress.Caption := Format('Parsing %s', [RegExprParsing.Match[1]]);
      end;
    end

    else if RegExprCompiling.Exec(TextLine) then
    begin
      if not FlagClosing then
      begin
        if ProgressBar.Position < ProgressBar.Max then
          ProgressBar.StepIt;
        StaticTextProgress.Caption := Format('Compiling %s', [RegExprCompiling.Match[1]]);
      end;
    end

    else if RegExprErrorParse.Exec(TextLine) then
    begin
      InfoError.TextFile    := '';
      InfoError.TextMessage := TextLine;
      InfoError.IndexLine   := 0;

      for IndexMatch := 1 to RegExprErrorParse.SubExprMatchCount do
      begin
        if (RegExprErrorParse.MatchLen[IndexMatch] > 0) and RegExprClass.Exec(RegExprErrorParse.Match[IndexMatch]) then
        begin
          Delete(InfoError.TextMessage, RegExprErrorParse.MatchPos[IndexMatch], RegExprErrorParse.MatchLen[IndexMatch]);
          Insert(RegExprErrorParse.Match[IndexMatch + 1], InfoError.TextMessage, RegExprErrorParse.MatchPos[IndexMatch]);
          InfoError.TextFile := IncludeTrailingBackslash(Configuration.DirPackage) + 'Classes\' + RegExprClass.Match[2] + '.uc';
          Break;
        end;
      end;

      ColorLine := clRed;
    end

    else if RegExprErrorCompile.Exec(TextLine) then
    begin
      InfoError.TextFile    :=          RegExprErrorCompile.Match[1];
      InfoError.TextMessage :=          RegExprErrorCompile.Match[3];
      InfoError.IndexLine   := StrToInt(RegExprErrorCompile.Match[2]);

      ColorLine := clRed;
      TextLine := FormatError('Error', InfoError);
    end

    else if RegExprWarningParse.Exec(TextLine) then
    begin
      InfoWarning := TInfoError.Create;
      ListInfoWarning.Add(InfoWarning);

      InfoWarning.TextMessage := TextLine;
      InfoWarning.TextFile    := '';
      InfoWarning.IndexLine   := 0;

      ColorLine := clRed;
    end

    else if RegExprWarningCompile.Exec(TextLine) then
    begin
      InfoWarning := TInfoError.Create;
      ListInfoWarning.Add(InfoWarning);

      InfoWarning.TextFile    :=          RegExprWarningCompile.Match[1];
      InfoWarning.TextMessage :=          RegExprWarningCompile.Match[3];
      InfoWarning.IndexLine   := StrToInt(RegExprWarningCompile.Match[2]);

      ColorLine := clRed;
      TextLine := FormatError('Warning', InfoWarning);
    end

    else if RegExprCompleted.Exec(TextLine) then
    begin
      StaticTextProgress.Caption := 'Finishing';
    end;

    RichEditMessagesAppend(TextLine, ColorLine);
  end;

  RichEditMessages.SelStart := RichEditMessages.Perform(EM_GETLIMITTEXT, 0, 0);
  RichEditMessages.Perform(EM_SCROLLCARET, 0, 0);
  RichEditMessages.Lines.EndUpdate;
end;


procedure TFormMain.PipedProcessTerminate(Sender: TObject);
begin
  if not FileExists(TextFilePackageOriginal) and FileExists(TextFilePackageBackup) then
    RenameFile(TextFilePackageBackup, TextFilePackageOriginal);

  if FlagClosing then
  begin
    Close;
  end
  else begin
    if PipedProcess.ExitCode = 0 then
    begin
      ProgressBar.Position := ProgressBar.Max;
      StaticTextProgress.Caption := 'Done';

      if ListInfoWarning.Count = 0 then
      begin
        Options.PerformAction(perfSuccess, Self, Configuration);
      end
      else begin
        Options.PerformAction(perfFailure, Self, Configuration);

        if Visible then
        begin
          UpdateDetailsWarning;
          PageControlDetails.ActivePage := TabSheetWarnings;
        end;
      end;
    end
    else begin
      StaticTextProgress.Caption := 'Failed';
      Options.PerformAction(perfFailure, Self, Configuration);

      if Visible then
      begin
        if Length(InfoError.TextMessage) > 0 then
        begin
          UpdateDetailsError;
          if ListInfoWarning.Count > 0 then
            UpdateDetailsWarning;
          PageControlDetails.ActivePage := TabSheetError;
        end

        else if ListInfoWarning.Count > 0 then
        begin
          UpdateDetailsWarning;
          PageControlDetails.ActivePage := TabSheetWarnings;
        end

        else begin
          if ButtonDetails.Enabled then
            ButtonDetailsClick(ButtonDetails);
        end;
      end;
    end;

    ButtonAbort.Caption := 'Close';
    ButtonAbort.Enabled := True;
    ButtonAbort.Cancel  := True;
  end;
end;


procedure TFormMain.RichEditMessagesAppend(TextAppend: string; ColorAppend: TColor);
begin
  RichEditMessages.SelStart := $7fffffff;
  RichEditMessages.SelAttributes.Color := ColorAppend;
  RichEditMessages.SelText := TextAppend + CRLF;
end;


procedure TFormMain.UpdateDetailsError;
begin
  ErrorDetails(InfoError, LabelErrorLocation, RichEditError);

  ButtonErrorEdit.Enabled := Length(InfoError.TextFile) > 0;
  TabSheetError.TabVisible := True;
end;


procedure TFormMain.UpdateDetailsWarning;
var
  ControlFocused: TControl;
begin
  ErrorDetails(ListInfoWarning[IndexWarning], LabelWarningLocation, RichEditWarning);
  LabelWarningNumber.Caption := Format('(%d of %d)', [IndexWarning + 1, ListInfoWarning.Count]);

  ControlFocused := ActiveControl;

  ButtonWarningPrev.Enabled := IndexWarning > 0;
  ButtonWarningNext.Enabled := IndexWarning < ListInfoWarning.Count - 1;

  if Assigned(ControlFocused) and not ControlFocused.Enabled then
    StaticTextProgress.SetFocus;

  ButtonWarningEdit.Enabled := Length(TInfoError(ListInfoWarning[IndexWarning]).TextFile) > 0;
  TabSheetWarnings.TabVisible := True;
end;


procedure TFormMain.ButtonWarningPrevClick(Sender: TObject);
begin
  if IndexWarning > 0 then
  begin
    Dec(IndexWarning);
    UpdateDetailsWarning;
  end;
end;


procedure TFormMain.ButtonWarningNextClick(Sender: TObject);
begin
  if IndexWarning < ListInfoWarning.Count - 1 then
  begin
    Inc(IndexWarning);
    UpdateDetailsWarning;
  end;
end;


procedure TFormMain.ButtonOptionsClick(Sender: TObject);
begin
  FormOptions.Configuration := Configuration;
  FormOptions.Options := Options;
  FormOptions.ShowModal;

  if not Visible then Close;
end;


procedure TFormMain.ButtonAbortClick(Sender: TObject);
begin
  Close;
end;


procedure TFormMain.ButtonDetailsClick(Sender: TObject);
begin
  Constraints.MaxWidth  := 0;
  Constraints.MaxHeight := 0;

  ClientHeight := ClientHeight + 248;
  Constraints.MinHeight := Constraints.MinHeight + 200;

  PageControlDetails.Height := ClientHeight - PageControlDetails.Top - PageControlDetails.Left;
  PageControlDetails.Enabled := True;

  ButtonDetails.Enabled := False;
end;


procedure TFormMain.ButtonErrorEditClick(Sender: TObject);
begin
  Options.PerformEdit(Configuration, InfoError.TextFile, InfoError.IndexLine);

  Hide;
  Close;
end;


procedure TFormMain.TabSheetErrorResize(Sender: TObject);
begin
  RichEditError.Repaint;
end;


procedure TFormMain.TabSheetWarningsResize(Sender: TObject);
begin
  RichEditWarning.Repaint;
end;


procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(PipedProcess) and PipedProcess.Executing then
  begin
    PipedProcess.Abort;

    ButtonAbort.Enabled := False;
    StaticTextProgress.Caption := 'Aborting';

    FlagClosing := True;
    CanClose := False;
  end
  else begin
    CanClose := not FormOptions.Visible;
  end;
end;


procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Configuration.Free;
  Options.Free;
  PipedProcess.Free;

  RegExprClass.Free;
  RegExprCompiling.Free;
  RegExprCompleted.Free;
  RegExprErrorCompile.Free;
  RegExprErrorParse.Free;
  RegExprPackage.Free;
  RegExprParsing.Free;
  RegExprWarningCompile.Free;
  RegExprWarningParse.Free;
end;

end.
