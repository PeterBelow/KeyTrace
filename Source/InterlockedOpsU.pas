{== InterlockedOpsU ===================================================}
{: This unit declares a number of typesafe wrappers for the Windows
  Interlocked* functions.
@author Dr. Peter Below
@desc   Version 1.0 created 2007-01-17<br/>
        Last modified       2007-01-17<p>
   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit InterlockedOpsU;

interface

function InterlockedExchangeObject(
  var Target; Value: TObject): TObject; stdcall;
function InterlockedExchangeHandle(
  var Target; Value: THandle): THandle; stdcall;
function InterlockedCompareExchangeObject(
  var Target; Exchange, Comperand: TObject): TObject; stdcall;
function InterlockedCompareExchangeHandle(
  var Target; Exchange, Comperand: THandle): THandle; stdcall;

implementation

uses Windows;

function InterlockedExchangeObject;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedExchange';
{$ENDIF}
function InterlockedExchangeHandle;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedExchange';
{$ENDIF}
function InterlockedCompareExchangeObject;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedCompareExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedCompareExchange';
{$ENDIF}
function InterlockedCompareExchangeHandle;
{$IFDEF WIN64}
  external kernel32 name 'InterlockedCompareExchangePointer';
{$ELSE}
  external kernel32 name 'InterlockedCompareExchange';
{$ENDIF}

end.
