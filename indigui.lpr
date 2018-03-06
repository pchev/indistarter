program indigui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  pu_indigui, Dialogs, Forms;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tf_indigui,f_indigui);
  Application.Run;
  if f_indigui.DisconnectedServer then ShowMessage('Disconnected from server');

end.

