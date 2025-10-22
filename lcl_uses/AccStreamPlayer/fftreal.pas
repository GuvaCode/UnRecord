unit FFTReal;

interface

uses
    Math, SysUtils;

type
    flt_t = Double;
    TFltArray = array of flt_t;
    TLongArray = array of LongInt;

    TBitReversedLUT = class
    private
      FLUT: TLongArray;
    public
      constructor Create(const nbr_bits: Integer);
      function GetIndex(index: Integer): LongInt;
    end;

    TTrigoLUT = class
    private
      FLUT: TFltArray;
      FOffsets: array of Integer;
    public
      constructor Create(const nbr_bits: Integer);
      function GetCos(i: Integer; level: Integer): flt_t;
      function GetSin(i: Integer; level: Integer): flt_t;
    end;

    TFFTReal = class
    private
      FBitRevLUT: TBitReversedLUT;
      FTrigoLUT: TTrigoLUT;
      FLength: LongInt;
      FNbrBits: Integer;
      FBuffer: TFltArray;
      FSqrt2_2: flt_t;

      procedure DoFFT4(f, x: TFltArray);
      procedure DoFFT2(f, x: TFltArray);
      procedure DoFFT1(f, x: TFltArray);
    public
      constructor Create(const length: LongInt);
      destructor Destroy; override;

      procedure Transform(f, x: TFltArray);
      procedure InverseTransform(f, x: TFltArray);
      procedure Rescale(x: TFltArray);

      property Length: LongInt read FLength;
    end;

implementation

{ TBitReversedLUT }

constructor TBitReversedLUT.Create(const nbr_bits: Integer);
var
  i, size, br_index, bit: Integer;
begin
  inherited Create;

  if nbr_bits <= 0 then
    size := 1
  else
    size := 1 shl nbr_bits;

  SetLength(FLUT, size);

  if size > 0 then
  begin
    FLUT[0] := 0;
    for i := 1 to size - 1 do
    begin
      br_index := FLUT[i - 1];
      bit := size shr 1;
      while (br_index and bit) <> 0 do
      begin
        br_index := br_index xor bit;
        bit := bit shr 1;
      end;
      br_index := br_index or bit;
      FLUT[i] := br_index;
    end;
  end;
end;

function TBitReversedLUT.GetIndex(index: Integer): LongInt;
begin
  if (index >= 0) and (index < System.Length(FLUT)) then
    Result := FLUT[index]
  else
    Result := index;
end;

{ TTrigoLUT }

constructor TTrigoLUT.Create(const nbr_bits: Integer);
var
  total_len, level, level_len, i, offset: Integer;
  PI, mul: Double;
begin
  inherited Create;

  if nbr_bits > 3 then
  begin
    total_len := (1 shl (nbr_bits - 1)) - 4;
    if total_len > 0 then
    begin
      SetLength(FLUT, total_len);
      SetLength(FOffsets, nbr_bits + 1);

      PI := System.Pi;
      offset := 0;

      for level := 3 to nbr_bits - 1 do
      begin
        FOffsets[level] := offset;
        level_len := 1 shl (level - 1);

        if (offset >= 0) and (offset + level_len <= total_len) then
        begin
          mul := PI / (level_len shl 1);
          for i := 0 to level_len - 1 do
          begin
            FLUT[offset + i] := System.Cos(i * mul);
          end;
        end;
        Inc(offset, level_len);
      end;
    end;
  end;
end;

function TTrigoLUT.GetCos(i: Integer; level: Integer): flt_t;
begin
  if (level >= 3) and (level < System.Length(FOffsets)) and
     (FOffsets[level] >= 0) and (i >= 0) and
     (i < (1 shl (level - 1))) then
  begin
    Result := FLUT[FOffsets[level] + i];
  end
  else
    Result := 0.0;
end;

function TTrigoLUT.GetSin(i: Integer; level: Integer): flt_t;
var
  level_len: Integer;
begin
  level_len := 1 shl (level - 1);
  if (i >= 0) and (i < level_len) then
    Result := GetCos(level_len - i, level)
  else
    Result := 0.0;
end;

{ TFFTReal }

constructor TFFTReal.Create(const length: LongInt);
begin
  inherited Create;

  if length <= 0 then
    raise Exception.Create('FFT length must be positive');

  FLength := length;
  FNbrBits := Trunc(Log2(length));

  if (1 shl FNbrBits) <> length then
    raise Exception.Create('FFT length must be power of 2');

  FBitRevLUT := TBitReversedLUT.Create(FNbrBits);
  FTrigoLUT := TTrigoLUT.Create(FNbrBits);
  FSqrt2_2 := Sqrt(2.0) * 0.5;

  SetLength(FBuffer, FLength);
end;

destructor TFFTReal.Destroy;
begin
  SetLength(FBuffer, 0);
  FBitRevLUT.Free;
  FTrigoLUT.Free;
  inherited;
end;

procedure TFFTReal.DoFFT4(f, x: TFltArray);
begin
  if (System.Length(f) >= 4) and (System.Length(x) >= 4) then
  begin
    f[1] := x[0] - x[2];
    f[3] := x[1] - x[3];

    f[0] := x[0] + x[2] + x[1] + x[3];
    f[2] := x[0] + x[2] - (x[1] + x[3]);
  end;
end;

procedure TFFTReal.DoFFT2(f, x: TFltArray);
begin
  if (System.Length(f) >= 2) and (System.Length(x) >= 2) then
  begin
    f[0] := x[0] + x[1];
    f[1] := x[0] - x[1];
  end;
end;

procedure TFFTReal.DoFFT1(f, x: TFltArray);
begin
  if (System.Length(f) >= 1) and (System.Length(x) >= 1) then
  begin
    f[0] := x[0];
  end;
end;

procedure TFFTReal.Transform(f, x: TFltArray);
var
  i, pass, nbr_coef, h_nbr_coef, d_nbr_coef, coef_index: Integer;
  sf, df: TFltArray;
  c, s, vr, vi: flt_t;
begin
  if (System.Length(f) <> FLength) or (System.Length(x) <> FLength) then
    Exit;

  // Special cases for small FFTs
  case FNbrBits of
    0: DoFFT1(f, x);
    1: DoFFT2(f, x);
    2: DoFFT4(f, x);
  else
    // For larger FFTs, use iterative algorithm
    if FNbrBits > 2 then
    begin
      // Initialize with bit-reversed order
      for i := 0 to FLength - 1 do
      begin
        f[i] := x[FBitRevLUT.GetIndex(i)];
      end;

      // Iterative FFT passes
      for pass := 1 to FNbrBits do
      begin
        nbr_coef := 1 shl pass;
        h_nbr_coef := nbr_coef shr 1;
        d_nbr_coef := nbr_coef shl 1;

        coef_index := 0;
        while coef_index < FLength do
        begin
          for i := 0 to h_nbr_coef - 1 do
          begin
            if coef_index + i + h_nbr_coef < FLength then
            begin
              c := FTrigoLUT.GetCos(i, pass);
              s := FTrigoLUT.GetSin(i, pass);

              vr := f[coef_index + i] + f[coef_index + i + h_nbr_coef];
              vi := f[coef_index + i] - f[coef_index + i + h_nbr_coef];

              FBuffer[coef_index + i] := vr;
              FBuffer[coef_index + i + h_nbr_coef] := vi * c + vr * s;
            end;
          end;
          Inc(coef_index, d_nbr_coef);
        end;

        // Swap buffers
        sf := f;
        f := FBuffer;
        FBuffer := sf;
      end;

      // If we ended up with buffer instead of f, copy back
      if f = FBuffer then
      begin
        for i := 0 to FLength - 1 do
          FBuffer[i] := f[i];
      end;
    end;
  end;
end;

procedure TFFTReal.InverseTransform(f, x: TFltArray);
var
  i: Integer;
begin
  // Simple inverse FFT (just copy for now)
  if (System.Length(f) = FLength) and (System.Length(x) = FLength) then
  begin
    for i := 0 to FLength - 1 do
      x[i] := f[i];
    Rescale(x);
  end;
end;

procedure TFFTReal.Rescale(x: TFltArray);
var
  i: Integer;
  mul: flt_t;
begin
  if System.Length(x) = FLength then
  begin
    mul := 1.0 / FLength;
    for i := 0 to FLength - 1 do
      x[i] := x[i] * mul;
  end;
end;

end.
