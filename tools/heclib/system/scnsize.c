#ifdef _MSC_VER
	#define _WIN32_WINNT 0x0500 /* Windows 2000 */
	#include <windows.h>
	typedef __int32 int32_t;
#else
	#include <termios.h>
	#include <unistd.h>
	#include <sys/ioctl.h>
	#include <stdint.h>
#endif

void scnsize_(int32_t *lines, int32_t *columns) {
	
#ifdef _MSC_VER
	HWND console_win = GetConsoleWindow();
	HDC console_dc = GetDC(console_win);
	int32_t console_map_mode = GetMapMode(console_dc);
	CONSOLE_FONT_INFO cfi;
	HANDLE std_out;
	COORD font_size;
	WINDOWINFO wi;
	/*-------------------------------------*/
	/* get the console font size in pixels */
	/*-------------------------------------*/
	if (console_map_mode != MM_TEXT) SetMapMode(console_dc, MM_TEXT);
	std_out = GetStdHandle(STD_OUTPUT_HANDLE);
	GetCurrentConsoleFont(std_out, FALSE, &cfi);
	font_size = GetConsoleFontSize(std_out, cfi.nFont);
	if (console_map_mode != MM_TEXT) SetMapMode(console_dc, console_map_mode);
	/*---------------------------------------*/
	/* get the console window size in pixels */
	/*---------------------------------------*/
	GetWindowInfo(console_win, &wi);
	/*---------------------------------------*/
	/* compute the window size in characters */
	/*---------------------------------------*/
	*lines   = (wi.rcClient.bottom - wi.rcClient.top) / font_size.Y;
	*columns = (wi.rcClient.right - wi.rcClient.left) / font_size.X;
#else
	struct winsize w;

	ioctl (1, TIOCGWINSZ, &w);
	*lines = w.ws_row;
	*columns = w.ws_col;
#endif
}

void scnlines_(int32_t *lines) {

	int32_t columns;
	scnsize_(lines, &columns);
}

void scncols_(int32_t *columns) {

	int32_t lines;
	scnsize_(&lines,columns);
}

