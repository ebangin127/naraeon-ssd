
// NSTDialog.h : 헤더 파일
//

#pragma once
#include "stdafx.h"

int operator== (const RECT a, const RECT b);

// CNSTDialog 클래스
// 이 클래스의 Property Getter/Setter들은 에러시 GetLastError()에 의거, Error Code를 던진다.
class CNSTDialog : public CDialogEx
{
private:
	int fWidth, fHeight, fX, fY, fClientWidth, fClientHeight;
	int fMaxWidth, fMaxHeight, fMinWidth, fMinHeight;
	int ClientDelta_X, ClientDelta_Y;
	RECT CurrWinRect, CurrClientRect;
	CBrush fBrush;
	COLORREF fBkColor;
public:
	CNSTDialog(UINT IDD, CWnd* pParent = NULL); // 표준 생성자입니다.

	// Property 관련 Getter
	// Constraints
	int GetMaxWidth();
	int GetMaxHeight();
	int GetMinWidth();
	int GetMinHeight();

	// Width/Height
	int GetWidth();
	int GetHeight();
	int GetClientWidth();
	int GetClientHeight();

	// X/Y
	int GetX();
	int GetY();

	// BkColor
	COLORREF GetBgColor();
	

	// Property 관련 Setter
	// Constraints
	int SetMaxWidth(int iWidth);
	int SetMaxHeight(int iHeight);
	int SetMinWidth(int iWidth);
	int SetMinHeight(int iHeight);

	// Width/Height
	int SetWidth(int iWidth);
	int SetHeight(int iHeight);
	int SetClientWidth(int iWidth);
	int SetClientHeight(int iHeight);

	// X/Y
	int SetX(int iX);
	int SetY(int iY);
	
	// BkColor
	COLORREF SetBgColor(COLORREF iBkColor);

protected:
	int RefreshWinVar();
};