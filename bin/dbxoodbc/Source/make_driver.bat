@set compiler=%1
@if "%compiler%" == "" set compiler=10

@rem compiler:
@rem 6  - Delphi  6
@rem 7  - Delphi  7
@rem 9  - Delphi  9 Win32
@rem 9  - Delphi 2005 Win32
@rem 10 - Delphi 2006 Win32
@rem 11 - Delphi 2007 Win32

@rem set release options
  @if not exist common.bat copy release.opt common.bat >nul

@call make_tools\make_prj.bat %compiler% dbxoodbc.dpr
