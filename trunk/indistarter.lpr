program indistarter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, pu_main, pu_devlist, sysutils, pu_setup;

{$R *.res}

begin
  {$ifdef USEHEAPTRC}
  DeleteFile('/tmp/indistarter_heap.trc');
  SetHeapTraceOutput('/tmp/indistarter_heap.trc');
  {$endif}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(Tf_devlist, f_devlist);
  Application.CreateForm(Tf_setup, f_setup);
  Application.Run;
end.

