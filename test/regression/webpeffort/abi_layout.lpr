program AbiLayout;
{$mode objfpc}{$H+}
uses libwebp;
var config: TWebPConfig; picture: TWebPPicture; stats: TWebPAuxStats; writer: TWebPMemoryWriter;
begin
  WriteLn('WebPConfig.sizeof=',SizeOf(config));
  WriteLn('WebPConfig.quality=',PtrUInt(@config.quality)-PtrUInt(@config));
  WriteLn('WebPConfig.method=',PtrUInt(@config.method)-PtrUInt(@config));
  WriteLn('WebPConfig.exact=',PtrUInt(@config.exact)-PtrUInt(@config));
  WriteLn('WebPConfig.qmax=',PtrUInt(@config.qmax)-PtrUInt(@config));
  WriteLn('WebPPicture.sizeof=',SizeOf(picture));
  WriteLn('WebPPicture.use_argb=',PtrUInt(@picture.use_argb)-PtrUInt(@picture));
  WriteLn('WebPPicture.y=',PtrUInt(@picture.y)-PtrUInt(@picture));
  WriteLn('WebPPicture.a=',PtrUInt(@picture.a)-PtrUInt(@picture));
  WriteLn('WebPPicture.argb=',PtrUInt(@picture.argb)-PtrUInt(@picture));
  WriteLn('WebPPicture.writer=',PtrUInt(@picture.writer)-PtrUInt(@picture));
  WriteLn('WebPPicture.custom_ptr=',PtrUInt(@picture.custom_ptr)-PtrUInt(@picture));
  WriteLn('WebPPicture.extra_info=',PtrUInt(@picture.extra_info)-PtrUInt(@picture));
  WriteLn('WebPPicture.stats=',PtrUInt(@picture.stats)-PtrUInt(@picture));
  WriteLn('WebPPicture.error_code=',PtrUInt(@picture.error_code)-PtrUInt(@picture));
  WriteLn('WebPPicture.progress_hook=',PtrUInt(@picture.progress_hook)-PtrUInt(@picture));
  WriteLn('WebPPicture.user_data=',PtrUInt(@picture.user_data)-PtrUInt(@picture));
  WriteLn('WebPPicture.pad4=',PtrUInt(@picture.pad4)-PtrUInt(@picture));
  WriteLn('WebPPicture.memory_=',PtrUInt(@picture.memory_)-PtrUInt(@picture));
  WriteLn('WebPPicture.memory_argb_=',PtrUInt(@picture.memory_argb_)-PtrUInt(@picture));
  WriteLn('WebPPicture.pad7=',PtrUInt(@picture.pad7)-PtrUInt(@picture));
  WriteLn('WebPAuxStats.sizeof=',SizeOf(stats));
  WriteLn('WebPAuxStats.PSNR=',PtrUInt(@stats.PSNR)-PtrUInt(@stats));
  WriteLn('WebPAuxStats.lossless_features=',PtrUInt(@stats.lossless_features)-PtrUInt(@stats));
  WriteLn('WebPAuxStats.pad=',PtrUInt(@stats.pad)-PtrUInt(@stats));
  WriteLn('WebPMemoryWriter.sizeof=',SizeOf(writer));
  WriteLn('WebPMemoryWriter.size=',PtrUInt(@writer.size)-PtrUInt(@writer));
  WriteLn('WebPMemoryWriter.max_size=',PtrUInt(@writer.max_size)-PtrUInt(@writer));
  WriteLn('WebPMemoryWriter.pad=',PtrUInt(@writer.pad)-PtrUInt(@writer));
end.
