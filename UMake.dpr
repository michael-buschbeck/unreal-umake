program UMake;


uses
  Forms,
  Controls,
  Windows,
  UMake_FormMain in 'UMake_FormMain.pas',
  UMake_FormOptions in 'UMake_FormOptions.pas',
  UMake_Configuration in 'UMake_Configuration.pas',
  UMake_Options in 'UMake_Options.pas',
  UMake_FormLaunch in 'UMake_FormLaunch.pas' {FormLaunch},
  UMake_FormShortcuts in 'UMake_FormShortcuts.pas' {FormShortcuts};

{$R *.RES}
{$R CursorHand.res}


begin
  Screen.Cursors[crHandPoint] := LoadCursor(HInstance, 'HANDCURSOR');

  Application.Initialize;
  Application.Title := 'UMake';
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormOptions, FormOptions);
  Application.CreateForm(TFormLaunch, FormLaunch);
  Application.CreateForm(TFormShortcuts, FormShortcuts);
  Application.ShowMainForm := False;
  FormMain.Startup;
  Application.Run;
end.
