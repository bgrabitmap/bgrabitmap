# WebP Decoder DLLs for BGRABitmap

This package provides precompiled DLLs for decoding WebP images. These DLLs are compiled directly from the official [libwebp](https://github.com/webmproject/libwebp) C library, with no wrapper included. The actual WebP reader/writer integration is handled by [BGRABitmap](https://github.com/bgrabitmap/bgrabitmap).

## 📦 Included Files

- `libwebp32.dll` — 32-bit version (dynamically linked, `/MD`)
- `libwebp64.dll` — 64-bit version (dynamically linked, `/MD`)
- `libwebp32_static.dll` *(otherwise)* — 32-bit version with statically linked C++ runtime (`/MT`)
- `libwebp64_static.dll` *(otherwise)* — 64-bit version with statically linked C++ runtime (`/MT`)

## 🔍 Dynamic vs Static

| DLL Name                | Architecture | Static Runtime | VC++ Redist Needed | Updatable | Notes                        |
|-------------------------|--------------|----------------|--------------------|-----------|------------------------------|
| `libwebp32.dll`           | 32-bit       | ❌             | ✅ Yes              | ✅ Yes    | Default 32-bit               |
| `libwebp64.dll`           | 64-bit       | ❌             | ✅ Yes              | ✅ Yes    | Default 64-bit               |
| `libwebp32_static.dll`    | 32-bit       | ✅ Yes         | ❌ No               | ❌ No     | Must be renamed if used      |
| `libwebp64_static.dll`    | 64-bit       | ✅ Yes         | ❌ No               | ❌ No     | Must be renamed if used      |

⚠️ If you use the static versions, you'll need to **rename** them to `libwebp32.dll` or `libwebp64.dll`, removing the suffix.

## 💡 Recommendation

- Use the **dynamic DLLs** (`libwebp32/64.dll`) if you want smaller files and automatic runtime updates (but VC++ redist is required).
- Use the **static versions** if you want to ensure it works out of the box, even on systems without the runtime.

## 🔧 Dependencies

For the dynamic versions, the following files must be present on the system:
- `vcruntime140.dll`
- `ucrtbase.dll`

Most Windows 10/11 systems include them by default. If needed, you can download the official Visual C++ Redistributable here:

📥 [https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist](https://learn.microsoft.com/en-us/cpp/windows/latest-supported-vc-redist)

### 🔁 Architecture-specific links:

- For **64-bit systems**: https://aka.ms/vs/17/release/vc_redist.x64.exe  
- For **32-bit systems**: https://aka.ms/vs/17/release/vc_redist.x86.exe

## 🧪 Error Handling Example (Free Pascal / Lazarus)

You can add a simple check using a `try...except` block to handle missing dependencies gracefully:

```pascal
uses BGRABitmap;

try
  bmp := TBGRABitmap.Create('image.webp');
  try
    // do something with the image    
  except
    // handle errors after the image is loaded
  end;
  Free;
except
  on E: Exception do
    ShowMessage('Failed to load image. This library requires the Microsoft Visual C++ Redistributable.' + LineEnding +
      'You can download it here:' + LineEnding +
        {$IFDEF CPU64}'https://aka.ms/vs/17/release/vc_redist.x64.exe'
        {$ELSE}       'https://aka.ms/vs/17/release/vc_redist.x86.exe'{$ENDIF});
end;
```
