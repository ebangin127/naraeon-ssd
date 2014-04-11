
// NSTDialog.cpp : 구현 파일
//
#include "NSTDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

//RECT 비교 연산자
int operator== (const RECT a, const RECT b) {
	return
		(a.left == b.left) && (a.right == b.right) &&
		(a.bottom == b.bottom) && (a.top == b.top);
}

// CNSTDialog 대화 상자
CNSTDialog::CNSTDialog(UINT IDD, CWnd* pParent): CDialogEx(IDD, pParent) {
	fMaxWidth  = 0;
	fMaxHeight = 0;
	fMinWidth  = 0;
	fMinHeight = 0;
}

//크기 혹은 XY값 변경시 Property에 적용해준다
int CNSTDialog::RefreshWinVar() {
	RECT WinRect, ClientRect;
	GetWindowRect(&WinRect);
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();

	GetClientRect(&ClientRect);
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();

	if((!(CurrWinRect == WinRect)) || (!(CurrClientRect == ClientRect))) {
		fWidth = WinRect.right - WinRect.left;
		fHeight = WinRect.bottom - WinRect.top;
		fClientWidth = ClientRect.right - ClientRect.left;
		fClientHeight = ClientRect.bottom - ClientRect.top;
		fX = WinRect.left;
		fY = WinRect.top;

		ClientDelta_X = fWidth - fClientWidth;
		ClientDelta_Y = fHeight - fClientHeight;

		CurrWinRect = WinRect;
		CurrClientRect = ClientRect;
	}

	return ERROR_SUCCESS;
}


//Getter
int CNSTDialog::GetMaxWidth() {
	return fMaxWidth;
}

int CNSTDialog::GetMaxHeight() {
	return fMaxHeight;
}

int CNSTDialog::GetMinWidth() {
	return fMinWidth;
}

int CNSTDialog::GetMinHeight() {
	return fMinHeight;
}

int CNSTDialog::GetWidth() {
	return fWidth;
}

int CNSTDialog::GetHeight() {
	RefreshWinVar();

	return fHeight;
}

int CNSTDialog::GetClientWidth() {
	RefreshWinVar();

	return fClientWidth;
}

int CNSTDialog::GetClientHeight() {
	RefreshWinVar();

	return fClientHeight;
}

int CNSTDialog::GetX() {
	RefreshWinVar();

	return fX;
}

int CNSTDialog::GetY() {
	RefreshWinVar();

	return fY;
}

COLORREF CNSTDialog::GetBgColor() {
	return fBkColor;
}

//Setter
int CNSTDialog::SetMaxWidth(int iWidth) {
	fMaxWidth = iWidth;
	if (GetWidth() > iWidth)
		SetWidth(iWidth);

	return iWidth;
}

int CNSTDialog::SetMaxHeight(int iHeight) {
	fMaxHeight = iHeight;
	if (GetHeight() > iHeight)
		SetHeight(iHeight);

	return iHeight;
}

int CNSTDialog::SetMinWidth(int iWidth) {
	fMinWidth = iWidth;
	if (GetWidth() < iWidth)
		SetWidth(iWidth);

	return iWidth;
}

int CNSTDialog::SetMinHeight(int iHeight) {
	fMinHeight = iHeight;
	if (GetHeight() < iHeight)
		SetHeight(iHeight);

	return iHeight;
}

int CNSTDialog::SetWidth(int iWidth) {
	RefreshWinVar();
	
	if(GetMaxWidth())      fWidth = (iWidth <= GetMaxWidth()) ? iWidth : GetMaxWidth();
	else if(GetMinWidth()) fWidth = (iWidth >= GetMinWidth()) ? iWidth : GetMinWidth();
	else fWidth = iWidth;

	fClientWidth = fWidth - ClientDelta_X;

	MoveWindow(GetX(), GetY(), iWidth, GetHeight());
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();

	return fWidth;
}

int CNSTDialog::SetHeight(int iHeight) {
	RefreshWinVar();
	
	if(GetMaxHeight())      fHeight = (iHeight <= GetMaxHeight()) ? iHeight : GetMaxHeight();
	else if(GetMinHeight()) fHeight = (iHeight >= GetMinHeight()) ? iHeight : GetMinHeight();
	else fHeight = iHeight;

	fClientHeight = fHeight - ClientDelta_Y;

	MoveWindow(GetX(), GetY(), GetWidth(), iHeight);
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();
	

	return fHeight;
}

int CNSTDialog::SetClientWidth(int iWidth) {
	RefreshWinVar();
	
	fWidth = iWidth + ClientDelta_X;

	if(GetMaxWidth())      fWidth = (fWidth <= GetMaxWidth()) ? fWidth : GetMaxWidth();
	else if(GetMinWidth()) fWidth = (fWidth >= GetMinWidth()) ? fWidth : GetMinWidth();

	fClientWidth = fWidth - ClientDelta_X;

	MoveWindow(GetX(), GetY(), iWidth + ClientDelta_X, GetHeight());
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();


	return fClientWidth;
}

int CNSTDialog::SetClientHeight(int iHeight) {
	RefreshWinVar();
	
	fHeight = iHeight + ClientDelta_Y;
	
	if(GetMaxHeight())      fHeight = (fHeight <= GetMaxHeight()) ? fHeight : GetMaxHeight();
	else if(GetMinHeight()) fHeight = (fHeight >= GetMinHeight()) ? fHeight : GetMinHeight();

	fClientHeight = fHeight - ClientDelta_Y;

	MoveWindow(GetX(), GetY(), GetWidth(), iHeight + ClientDelta_Y);
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();


	return fClientHeight;
}

int CNSTDialog::SetX(int iX) {
	RefreshWinVar();

	MoveWindow(iX, GetY(), GetWidth(), GetHeight());
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();
	
	fX = iX;

	return iX;
}

int CNSTDialog::SetY(int iY) {
	RefreshWinVar();

	MoveWindow(GetX(), iY, GetWidth(), GetHeight());
	if(GetLastError() != ERROR_SUCCESS)
		throw GetLastError();
	
	fY = iY;

	return iY;
}

COLORREF CNSTDialog::SetBgColor(COLORREF iBkColor) {
	fBkColor = iBkColor;
	SetBackgroundColor(iBkColor);
	return fBkColor;
}