unit pu_setup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls;

type

  { Tf_setup }

  Tf_setup = class(TForm)
    autostart: TCheckBox;
    Button1: TButton;
    devlist: TFileNameEdit;
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  f_setup: Tf_setup;

implementation

{$R *.lfm}

{ Tf_setup }

end.

