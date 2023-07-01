unit Radio.Shared;

interface

const
  strLoading:String=' Loading...';
  strUnknown:String= 'Unknown!';
  strCompleted:String= 'Completed!';

type
    HSTREAM = DWORD;      // sample stream handle
    TFFTData  = array [0..512] of Single;
    TStatusProc        = procedure(pszData : string; Progress: Integer; Chanel: HSTREAM);
    TBroadcastInfoProc = procedure(pszBroadcastName,pszBitRate:string);
    TBroadcastMetaProc = procedure(pszData : string);

implementation

end.
