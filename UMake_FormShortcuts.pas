unit UMake_FormShortcuts;


interface


uses
  UMake_Configuration,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, SysTools, Shortcuts, Registry, ExtCtrls, FileCtrl;


(*****************************************************************************)
(*  TFormShortcuts
(*****************************************************************************)

type
  TFormShortcuts = class(TForm)
    BevelAuto: TBevel;
    BevelGeneric: TBevel;
    BevelProject: TBevel;
    ButtonBrowseGame: TBitBtn;
    ButtonCancel: TButton;
    ButtonCreate: TButton;
    ComboBoxGame: TComboBox;
    LabelExplanationAuto: TLabel;
    LabelExplanationGeneric: TLabel;
    LabelExplanationProject: TLabel;
    LabelProject: TLabel;
    PanelFocus: TPanel;
    RadioButtonAuto: TRadioButton;
    RadioButtonGeneric: TRadioButton;
    RadioButtonProject: TRadioButton;

    procedure FormShow(Sender: TObject);
    procedure ButtonBrowseGameClick(Sender: TObject);
    procedure ButtonCreateClick(Sender: TObject);
    procedure ComboBoxGameChange(Sender: TObject);

  public
    Configuration: TConfiguration;

  private
    procedure CreateShortcutGeneric;
    procedure CreateShortcutProject;
    procedure CreateShortcutAuto;
  end;


var
  FormShortcuts: TFormShortcuts;


implementation


{$R *.DFM}


(*****************************************************************************)
(*  TFormShortcuts
(*****************************************************************************)

procedure TFormShortcuts.FormShow(Sender: TObject);
var
  IndexKey: Integer;
  Registry: TRegistry;
  StringListKeys: TStringList;
  TextDirGame: string;
begin
  PanelFocus.SetFocus;
  RadioButtonGeneric.Checked := True;

  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_LOCAL_MACHINE;

  if Registry.OpenKeyReadOnly('\SOFTWARE\Unreal Technology\Installed Apps') then
  begin
    StringListKeys := TStringList.Create;
    Registry.GetKeyNames(StringListKeys);
    Registry.CloseKey;

    for IndexKey := 0 to StringListKeys.Count - 1 do
    begin
      if Registry.OpenKeyReadOnly('\SOFTWARE\Unreal Technology\Installed Apps\' + StringListKeys[IndexKey]) then
      begin
        if Registry.ValueExists('Folder') then
        begin
          TextDirGame := Registry.ReadString('Folder');
          TextDirGame := GetLongPath(TextDirGame);
          if FileExists(IncludeTrailingBackslash(TextDirGame) + 'System\ucc.exe') and (ComboBoxGame.Items.IndexOf(TextDirGame) < 0) then
            ComboBoxGame.Items.Add(ExcludeTrailingBackslash(TextDirGame));
        end;
        Registry.CloseKey;
      end;
    end;

    if ComboBoxGame.Items.Count > 0 then
      ComboBoxGame.Text := ComboBoxGame.Items[0];

    StringListKeys.Free;
  end;

  Registry.Free;

  if Assigned(Configuration) then
  begin
    LabelProject.Caption := Format('for %s', [Configuration.Package]);
    LabelExplanationProject.Caption := Format('Double-click this shortcut to directly compile the currently loaded project, %s.', [Configuration.Package]);
    BevelProject.SetBounds(LabelProject.Left + LabelProject.Width + 5, BevelProject.Top, BevelProject.Left + BevelProject.Width - LabelProject.Left - LabelProject.Width - 5, BevelProject.Height);
    ComboBoxGame.Text := ExcludeTrailingBackslash(Configuration.DirGame);
  end
  else begin
    RadioButtonProject.Enabled := False;
    LabelProject.Hide;
    LabelExplanationProject.Enabled := False;
    LabelExplanationProject.Caption := 'Load a project first to enable this option.';
    BevelProject.SetBounds(LabelProject.Left + 2, BevelProject.Top, BevelProject.Left + BevelProject.Width - LabelProject.Left - 2, BevelProject.Height);
  end;
end;


procedure TFormShortcuts.ButtonBrowseGameClick(Sender: TObject);
var
  TextDirGame: string;
begin
  TextDirGame := BrowseFolder(Handle, 'Select the base directory of the game UMake should search for recently modified projects:');

  ComboBoxGame.Text := ExcludeTrailingBackslash(TextDirGame);
  ComboBoxGameChange(ComboBoxGame);
  ComboBoxGame.SetFocus;
end;


procedure TFormShortcuts.ComboBoxGameChange(Sender: TObject);
begin
  RadioButtonAuto.Checked := True;
end;

procedure TFormShortcuts.ButtonCreateClick(Sender: TObject);
begin
       if RadioButtonGeneric.Checked then CreateShortcutGeneric
  else if RadioButtonProject.Checked then CreateShortcutProject
  else if RadioButtonAuto   .Checked then CreateShortcutAuto;
end;


procedure TFormShortcuts.CreateShortcutGeneric;
var
  ShortcutDesktop: TFileShortcut;
begin
  ShortcutDesktop := TFileShortcut.Create;
  ShortcutDesktop.Path := GetLongPath(ParamStr(0));
  ShortcutDesktop.Description := 'Compile an UnrealScript file by dropping it on this icon.';
  ShortcutDesktop.Save(IncludeTrailingBackslash(GetDesktopPath) + 'UMake.lnk');
  ShortcutDesktop.Free;
end;


procedure TFormShortcuts.CreateShortcutProject;
var
  ShortcutDesktop: TFileShortcut;
begin
  ShortcutDesktop := TFileShortcut.Create;
  ShortcutDesktop.Path := GetLongPath(ParamStr(0));
  ShortcutDesktop.Arguments := GetQuotedParam(Configuration.DirPackage);
  ShortcutDesktop.Description := Format('Double-click this icon to compile %s.', [Configuration.Package]);
  ShortcutDesktop.Save(IncludeTrailingBackslash(GetDesktopPath) + Format('Compile %s.lnk', [Configuration.Package]));
  ShortcutDesktop.Free;
end;


procedure TFormShortcuts.CreateShortcutAuto;
var
  ShortcutDesktop: TFileShortcut;
  TextDirGame: string;
begin
  TextDirGame := GetLongPath(ComboBoxGame.Text);

  if FileExists(IncludeTrailingBackslash(TextDirGame) + 'System\ucc.exe') then
  begin
    ShortcutDesktop := TFileShortcut.Create;
    ShortcutDesktop.Path := GetLongPath(ParamStr(0));
    ShortcutDesktop.Arguments := Format('/auto %s', [GetQuotedParam(TextDirGame)]);
    ShortcutDesktop.Description := Format('Double-click this icon to compile the most recently modified project in %s.', [ExtractFileName(ComboBoxGame.Text)]);
    ShortcutDesktop.Save(IncludeTrailingBackslash(GetDesktopPath) + Format('Compile %s Project.lnk', [ExtractFileName(ComboBoxGame.Text)]));
    ShortcutDesktop.Free;
  end
  else begin
    if DirectoryExists(TextDirGame)
      then Application.MessageBox('Invalid game directory.'#13#10#13#10'The game directory you selected seems to be invalid (no compiler found). Maybe you have to download and install a developer''s toolkit first.', PChar(Application.Title), MB_ICONERROR)
      else Application.MessageBox('The selected game directory doesn''t exist.', PChar(Application.Title), MB_ICONERROR);

    ComboBoxGame.SetFocus;
    ModalResult := mrNone;
  end;
end;


end.
