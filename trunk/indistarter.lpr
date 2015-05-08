program indistarter;

{$mode objfpc}{$H+}

{
Copyright (C) 2015 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>. 

}

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

