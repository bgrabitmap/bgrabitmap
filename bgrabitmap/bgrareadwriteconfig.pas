// SPDX-License-Identifier: LGPL-3.0-linking-exception

{
  2024-01  - Massimo Magnano:
           - Definition of Image format Reader/Writer Configuration Interface and Base Class

}
unit BGRAReadWriteConfig;

{$mode objfpc}
{$H+}

{$interfaces corba}
{ #note -oMaxM :
  In this way any Class, even if it does not derive from TInterfacedObject, can implement the interface }

interface

uses
  BGRAClasses;

type
  { TBGRAReadWriteConfig }

  { Base Class that contain Reader/Writer Configuration Data

    #note -oMaxM :
    Derived from TPersistent so we can directly store published properties in a Stream/XML }
  TBGRAReadWriteConfig = class(TPersistent)

  end;
  TBGRAReadWriteConfigClass = class of TBGRAReadWriteConfig;

  { IBGRAReadWriteConfig }

  { #note -oMaxM :
    Every Reader/Writer should implement this interface,
    so we can maintain the configuration between reading and writing }
  IBGRAReadWriteConfig = interface
    //Copy Configuration from the Reader/Writer Class to TBGRAReadWriteConfig
    function GetBGRAReadWriteConfig: TBGRAReadWriteConfig;

    //Copy Configuration from TBGRAReadWriteConfig to Reader/Writer Class
    function SetBGRAReadWriteConfig(ASource: TBGRAReadWriteConfig): Boolean;
  end;

implementation

end.

