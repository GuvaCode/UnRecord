unit RadioDownloads;

{$mode delphi}

interface

uses
  SysUtils, DateUtils,
  Codebot.System;

{ IRadioDownload }

type
  IRadioDownload = interface
  ['{7F235DB8-56FD-4538-9020-4B7E37F4C98A}']
    function GetId: Integer;
    function GetPrefix: string;
    function GetTitle: string;
    function GetTooltip: string;
    function GetSort: Integer;
    function GetBgImage: string;
    function GetBgImageMobile: string;
    function GetSvgOutline: string;
    function GetSvgFill: string;
    function GetShortTitle: string;
    function GetIsNew: Boolean;
    function GetStream64: string;
    function GetStream128: string;
    function GetStream320: string;
    function GetUpdated: string;

    property Id: Integer read GetId;
    property Prefix: string read GetPrefix;
    property Title: string read GetTitle;
    property Tooltip: string read GetTooltip;
    property Sort: Integer read GetSort;
    property BgImage: string read GetBgImage;
    property BgImageMobile: string read GetBgImageMobile;
    property SvgOutline: string read GetSvgOutline;
    property SvgFill: string read GetSvgFill;
    property ShortTitle: string read GetShortTitle;
    property IsNew: Boolean read GetIsNew;
    property Stream64: string read GetStream64;
    property Stream128: string read GetStream128;
    property Stream320: string read GetStream320;
    property Updated: string read GetUpdated;
  end;

function NewRadioDownload(AId: Integer; APrefix, ATitle, ATooltip: string;
  ASort: Integer; ABgImage, ABgImageMobile, ASvgOutline, ASvgFill, AShortTitle: string;
  AIsNew: Boolean; AStream64, AStream128, AStream320, AUpdated: string): IRadioDownload;

type
  TRadioDownloadList = TArrayList<IRadioDownload>;
  TRadioDownloadCompare = TCompare<IRadioDownload>;

const
  tagName = 0;
  tagNew = tagName + 1;
  tagUpdated = tagNew + 1;
  tagSort = tagUpdated + 1; // Добавлена константа для сортировки по популярности

function RadioDownloadCompare(Tag: Integer): TRadioDownloadCompare;

implementation

{ TRadioDownload }

type
  TRadioDownload = class(TInterfacedObject, IRadioDownload)
  private
    FId: Integer;
    FPrefix: string;
    FTitle: string;
    FTooltip: string;
    FSort: Integer;
    FBgImage: string;
    FBgImageMobile: string;
    FSvgOutline: string;
    FSvgFill: string;
    FShortTitle: string;
    FIsNew: Boolean;
    FStream64: string;
    FStream128: string;
    FStream320: string;
    FUpdated: string;
  public
    constructor Create(AId: Integer; APrefix, ATitle, ATooltip: string;
      ASort: Integer; ABgImage, ABgImageMobile, ASvgOutline, ASvgFill, AShortTitle: string;
      AIsNew: Boolean; AStream64, AStream128, AStream320, AUpdated: string);
    function GetId: Integer;
    function GetPrefix: string;
    function GetTitle: string;
    function GetTooltip: string;
    function GetSort: Integer;
    function GetBgImage: string;
    function GetBgImageMobile: string;
    function GetSvgOutline: string;
    function GetSvgFill: string;
    function GetShortTitle: string;
    function GetIsNew: Boolean;
    function GetStream64: string;
    function GetStream128: string;
    function GetStream320: string;
    function GetUpdated: string;
  end;

constructor TRadioDownload.Create(AId: Integer; APrefix, ATitle, ATooltip: string;
  ASort: Integer; ABgImage, ABgImageMobile, ASvgOutline, ASvgFill, AShortTitle: string;
  AIsNew: Boolean; AStream64, AStream128, AStream320, AUpdated: string);
begin
  inherited Create;
  FId := AId;
  FPrefix := APrefix;
  FTitle := ATitle;
  FTooltip := ATooltip;
  FSort := ASort;
  FBgImage := ABgImage;
  FBgImageMobile := ABgImageMobile;
  FSvgOutline := ASvgOutline;
  FSvgFill := ASvgFill;
  FShortTitle := AShortTitle;
  FIsNew := AIsNew;
  FStream64 := AStream64;
  FStream128 := AStream128;
  FStream320 := AStream320;
  FUpdated := AUpdated;
end;

function TRadioDownload.GetId: Integer;
begin
  Result := FId;
end;

function TRadioDownload.GetPrefix: string;
begin
  Result := FPrefix;
end;

function TRadioDownload.GetTitle: string;
begin
  Result := FTitle;
end;

function TRadioDownload.GetTooltip: string;
begin
  Result := FTooltip;
end;

function TRadioDownload.GetSort: Integer;
begin
  Result := FSort;
end;

function TRadioDownload.GetBgImage: string;
begin
  Result := FBgImage;
end;

function TRadioDownload.GetBgImageMobile: string;
begin
  Result := FBgImageMobile;
end;

function TRadioDownload.GetSvgOutline: string;
begin
  Result := FSvgOutline;
end;

function TRadioDownload.GetSvgFill: string;
begin
  Result := FSvgFill;
end;

function TRadioDownload.GetShortTitle: string;
begin
  Result := FShortTitle;
end;

function TRadioDownload.GetIsNew: Boolean;
begin
  Result := FIsNew;
end;

function TRadioDownload.GetStream64: string;
begin
  Result := FStream64;
end;

function TRadioDownload.GetStream128: string;
begin
  Result := FStream128;
end;

function TRadioDownload.GetStream320: string;
begin
  Result := FStream320;
end;

function TRadioDownload.GetUpdated: string;
begin
  Result := FUpdated;
end;

{ Sorting functions }

function SortByName(constref A, B: IRadioDownload): Integer;
begin
  Result := CompareText(A.Title, B.Title);
end;

function SortByNew(constref A, B: IRadioDownload): Integer;
begin
  // Новые станции сначала, затем по имени
  if A.IsNew and not B.IsNew then
    Result := -1
  else if not A.IsNew and B.IsNew then
    Result := 1
  else
    Result := SortByName(A, B);
end;

function SortByUpdated(constref A, B: IRadioDownload): Integer;
var
  DateA, DateB: TDateTime;
begin
  // Пытаемся преобразовать строку в дату
  try
    DateA := ScanDateTime('dd.mm.yyyy hh:nn:ss', A.Updated);
  except
    DateA := 0;
  end;

  try
    DateB := ScanDateTime('dd.mm.yyyy hh:nn:ss', B.Updated);
  except
    DateB := 0;
  end;

  // Сначала более новые даты
  if DateA > DateB then
    Result := -1
  else if DateA < DateB then
    Result := 1
  else
    Result := SortByName(A, B);
end;

function SortBySort(constref A, B: IRadioDownload): Integer;
begin
  // Сортировка по полю Sort (популярность), меньшие значения сначала
  if A.Sort < B.Sort then
    Result := -1
  else if A.Sort > B.Sort then
    Result := 1
  else
    Result := SortByName(A, B);
end;

function RadioDownloadCompare(Tag: Integer): TRadioDownloadCompare;
begin
  case Tag of
    tagName: Result := SortByName;
    tagNew: Result := SortByNew;
    tagUpdated: Result := SortByUpdated;
    tagSort: Result := SortBySort; // Добавлена сортировка по популярности
  else
    Result := SortByName; // По умолчанию сортировка по имени
  end;
end;

function NewRadioDownload(AId: Integer; APrefix, ATitle, ATooltip: string;
  ASort: Integer; ABgImage, ABgImageMobile, ASvgOutline, ASvgFill, AShortTitle: string;
  AIsNew: Boolean; AStream64, AStream128, AStream320, AUpdated: string): IRadioDownload;
begin
  Result := TRadioDownload.Create(AId, APrefix, ATitle, ATooltip, ASort,
    ABgImage, ABgImageMobile, ASvgOutline, ASvgFill, AShortTitle,
    AIsNew, AStream64, AStream128, AStream320, AUpdated);
end;

end.
