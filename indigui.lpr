program indigui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  pu_indigui, u_utils, Dialogs, Forms;

{$R *.res}
{$i revision.inc}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tf_indigui,f_indigui);
  f_indigui.Caption:='INDI Client '+starter_version+'-'+RevisionStr;
  Application.Run;
  if f_indigui.DisconnectedServer then ShowMessage('Disconnected from server');

end.

