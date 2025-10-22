{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AccStreamPLayer;

{$warn 5023 off : no warning about unused units}
interface

uses
  AACPlayer, httpgetthread, metadatathread, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AACPlayer', @AACPlayer.Register);
end;

initialization
  RegisterPackage('AccStreamPLayer', @Register);
end.
