{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CodeBotEx;

{$warn 5023 off : no warning about unused units}
interface

uses
  Slider, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Slider', @Slider.Register);
end;

initialization
  RegisterPackage('CodeBotEx', @Register);
end.
