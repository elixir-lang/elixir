#include <stdio.h>
#include <windows.h>

int main(void) {
    const int W_BLACK     = 0;
    const int W_BLUE      = 1;
    const int W_GREEN     = 2;
    const int W_RED       = 4;
    const int W_INTENSITY = 8;
    const int W_CYAN      = W_BLUE | W_GREEN;
    const int W_MAGENTA   = W_BLUE | W_RED;
    const int W_YELLOW    = W_GREEN | W_RED;
    const int W_WHITE     = W_BLUE | W_GREEN | W_RED;
    const int ANSI2WIN[]  = {W_BLACK, W_RED, W_GREEN, W_YELLOW, W_BLUE, W_MAGENTA, W_CYAN, W_WHITE};

    const int RESET   =  0;
    const int BOLD    =  1;
    const int BLACK   = 30;
    const int RED     = 31;
    const int GREEN   = 32;
    const int YELLOW  = 33;
    const int BLUE    = 34;
    const int MAGENTA = 35;
    const int CYAN    = 36;
    const int GRAY    = 37;
    const int WHITE   = 37;

    CONSOLE_SCREEN_BUFFER_INFO consoleInfo;
    GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &consoleInfo);
    int initialColor = consoleInfo.wAttributes;

    int state, ansiColor, multiplication, pos, winColor, winIntensity = W_BLACK;
    int colors[] = {-1,-1};
    int colorPos = 1;
    int ch;

    while ((ch = getchar()) != EOF) {
        if(ch == '\e') {
            state = '\e';
        } else if (state == '\e' && ch == '[') {
            state = '[';
        } else if (state == '[') {
            if (ch != 'm') {
                colors[colorPos] = ch;
                colorPos--;
            } else {
                // Find ANSI Color
                ansiColor = 0;
                multiplication = 1;
                for (pos = colorPos + 1; pos < 2; pos++) {
                    ansiColor += (colors[pos] - 48) * multiplication;
                    multiplication *= 10;
                }

                // Convert ANSI Color to Windows Color
                if (ansiColor == BOLD) {
                    winIntensity = W_INTENSITY;
                } else if (ansiColor == RESET) {
                    winIntensity = W_BLACK;
                    winColor = initialColor;
                } else if (BLACK <= ansiColor && ansiColor <= WHITE) {
                    winColor = ANSI2WIN[ansiColor - 30];
                } else if (ansiColor == 90) {
                    // Special case for gray (it's really white)
                    winColor = W_WHITE;
                    winIntensity = W_BLACK;
                }

                // initialColor & 0xF0 is to keep the background color
                SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), winColor | winIntensity | (initialColor & 0xF0));
                colorPos = 1;
                state = -1;
            }
		} else {
            putchar(ch);
		}
	}

    SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), initialColor);
    return 0;
}
