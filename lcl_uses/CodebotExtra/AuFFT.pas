{*******************************************************}
{                                                       }
{       Audorra Digital Audio Library                   }
{       Copyright (c) Andreas Stöckel, 2011             }
{       Audorra is an "Andorra Suite" Project           }
{                                                       }
{*******************************************************}

{The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Initial Developer of the Original Code is
Andreas Stöckel. All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License license (the “GPL License”), in which case the provisions of
GPL License are applicable instead of those above. If you wish to allow use
of your version of this file only under the terms of the GPL License and not
to allow others to use your version of this file under the MPL, indicate your
decision by deleting the provisions above and replace them with the notice and
other provisions required by the GPL License. If you do not delete the
provisions above, a recipient may use your version of this file under either the
MPL or the GPL License.

File: AuFFT.pas
Author: Andreas Stöckel
}

unit AuFFT;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils,

  AuComplex;

type
  //Used for tabulating all coeffizients which occur inside the radix-2-FFT algorithm
  TAuFFTTable = record
    lin: Integer; //< Where to read the left input sample from
    rin: Integer; //< Where to read the right input sample from
    lout: Integer; //< Where to write the left output sample to
    rout: Integer; //< Where to write the right output sample to
    phi: TAuComplex; //< Constant part in the FFT equation: e^(-i PI (m / n))
  end;

  {EAuFFTNonPow2 is raised whenever }
  EAuFFTNonPow2 = class(Exception);

  { TAuFFTransformer }

  {TAuFFTransformer performs a radix-2 1D fast fourier transform.}
  TAuFFTransformer = class
    private
      FTable: array of TAuFFTTable;
      FTmpBuf: PAuComplex;
      FTmpBufSize: Integer;
      FSize: Integer;
      FSteps: Integer;
      FDepth: Integer;
      procedure ConstructTable;
    public
      {Creates an instance of TAuFFTransformer. Upon calling this function a table
       will be created which contains all information, which has to be used each
       processing step.

       @param(AFFTSize must be a power of two integer. Recommended are sizes from
         2-16384 samples. On a Intel Core2Duo 2,66GHz the algorithm can process up
         to 7 Mio complex samples per second (on one core).)}
      constructor Create(AFFTSize: Integer);
      {Destroys TAuFFTransformer.}
      destructor Destroy;override;

      {Actually performs the fast fourier transformation. The data will be read and
       written to AMem. If you don't want to overwrite your source data, you can
       use the second overloaded version of DoFFT, which has a ASrc and ADst parameter.

       DoFFT will read/write FFTSize * SizeOf(TAuComplex) bytes. Make sure that
       you have reserved and initialized at least that amount of memory.

       If you want to use one and the same FFT object from multiple threads, make
       sure, that you only use the two parameter version of DoFFT as the first
       version uses an class-internal buffer which may only be accessed by one
       thread at a time.}
      procedure DoFFT(AMem: PAuComplex);overload;
      procedure DoFFT(ASrc, ADst: PAuComplex);overload;

      property FFTSize: Integer read FSize;
  end;

implementation

//Returns whether the given number is power of two
function IsPow2(n: integer): boolean; inline;
begin
  Result := (n - 1) and n = 0;
end;

//Returns the binary logarithm of n
function IntLog2(n: integer): integer;
var
  i, b: integer;
begin
  result := 0;
  for i := 4 downto 0 do
  begin
    b := 1 shl i;
    if n >= 1 shl b then
    begin
      n := n shr b;
      result := result + b;
    end;
  end;
end;

{ TAuFFTransformer }

constructor TAuFFTransformer.Create(AFFTSize: Integer);
begin
  inherited Create;

  //Check whether the passed parameter is a power of two value
  if (not IsPow2(AFFTSize) and (AFFTSize > 0)) then
    raise EAuFFTNonPow2.Create('The given FFT size is non power of two!');

  //Copy the size parameter, calculate the recursion depth and the size of the
  //table
  FSize := AFFTSize;
  FDepth := IntLog2(FSize);
  FSteps := FDepth * (1 shl (FDepth - 1));

  //Calculate the size of the temporary output buffer (the buffery itself will
  //be created when it is needed.)
  FTmpBufSize := SizeOf(TAuComplex) * AFFTSize;

  ConstructTable;
end;

destructor TAuFFTransformer.Destroy;
begin
  //Free the memory reserved for the temporary buffer
  if FTmpBuf <> nil then
    FreeMem(FTmpBuf);

  FTmpBuf := nil;
  FTmpBufSize := 0;

  inherited Destroy;
end;

procedure TAuFFTransformer.ConstructTable;

  //For the first "recursion"-step the indices have to be reordered:
  //    0 1 2 3 4 5 6 7
  //--> 0 4 2 6 1 5 3 7
  //The reorder_idx function takes the initial index and the "recursion" depth
  function reorder_idx(idx, depth: integer): integer;
  var
    i:     integer;
    digit: boolean;
  begin
    Result := 0;
    i := 0;
    while idx > 0 do
    begin
      if idx mod 2 = 1 then
        Result := Result or (1 shl (depth - i - 1));
      idx := idx div 2;
      i := i + 1;
    end;
  end;

var
  lbase, rbase: Integer;
  rec_depth, rec_section, rec_elem: Integer;
  theta: Single;
  inv_d: single;
  i: Integer;
begin
  SetLength(FTable, FSteps);
  i := 0;

  //The outer loop represents the recursion setps done in the recursive variant
  //of the FFT algorithm. We start at the lowest level, where every "section"
  //of entrys only contains of a pair of complex numbers.
  for rec_depth := 0 to FDepth - 1 do
  begin

    rbase := (1 shl rec_depth);
    inv_d := - PI / rbase;

    //The middle loop represents the "sections" the input vector is divided into.
    //We start with sections with two elements and stop when the section contains
    //the whole input vector.
    for rec_section := 0 to (1 shl (FDepth - rec_depth - 1)) - 1 do
    begin
      lbase := 2 * rbase * rec_section;

      //The inner loop goes over the elements of each section. In order to construct
      //the table, the read and write indices, as well as the constant theta complex
      //number are calculated.
      for rec_elem := 0 to (1 shl rec_depth) - 1 do
      begin
        with FTable[i] do
        begin
          lout := lbase + rec_elem;
          rout := rbase + lout;

          //The first recursion step is used to reorder the input elements. This
          //represents taking the odd or even elements in the recursive algorithm.
          if (rec_depth = 0) then
          begin
            lin := reorder_idx(lout, FDepth);
            rin := reorder_idx(rout, FDepth);
          end
          else
          begin
            lin := lout;
            rin := rout;
          end;

          theta := rec_elem * inv_d;
          phi.re := Cos(theta);
          phi.im := Sin(theta);
        end;
        inc(i);
      end;
    end;
  end;
end;

procedure TAuFFTransformer.DoFFT(AMem: PAuComplex);
begin
  //Initialize the temporary buffer if this hasn't been done yet.
  if (FTmpBuf = nil) then
    FTmpBuf := GetMem(FTmpBufSize);

  //Perform the actual FFT
  DoFFT(AMem, FTmpBuf);

  //Overwrite the input buffer
  Move(FTmpBuf^, AMem^, FTmpBufSize);
end;

procedure TAuFFTransformer.DoFFT(ASrc, ADst: PAuComplex);
var
  i: integer;
  pin, pout: PAuComplexField;
  zl, zr, zz: TAuComplex;
  t1, t2: Double;
begin
  pin := PAuComplexField(ASrc);
  pout := PAuComplexField(ADst);
  for i := 0 to FSteps - 1 do
  begin
    //Switch input to output buffer after first reordering pass
    if (i = FSize div 2) then
      pin := pout;

    //Fetch the coefficients from the current table entry
    with FTable[i] do
    begin
      //Read the "left" and the "right" sample from the input buffer
      zl := pin^[lin];
      zr := pin^[rin];

      //Calculate zz = zl * phi
      zz.re := (zl.re * phi.re) - (zl.im * phi.im);
      zz.im := (zl.re * phi.im) + (zl.im * phi.re);

      //Calculate zl = zr + zz
      zl.re := zr.re + zz.re;
      zl.im := zr.im + zz.im;

      //Calculate zr = rr - zz
      zr.re := zr.re - zz.re;
      zr.im := zr.im - zz.im;

      //Write the left and the right sample to the output buffer
      pout^[lout] := zl;
      pout^[rout] := zr;
    end;
  end;
end;

end.

