#ifndef _CLIENT_H_
#define _CLIENT_H_

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>
#include <fnp.h>
#include <assert.h>

#ifdef __unix__
#define MKDIR(x) mkdir(x,493)
#else /* __unix__ */
#define MKDIR(x) mkdir(x)
#endif /* __unix__ */

extern GtkWidget *wnd_main;
extern GtkCList *FNP_clist;
extern int FNP_CurrentSong;

void PlayNextFile(bool Lock);

void OnEndOfFile();
void OnEndTCPThread(void);
void OnError(int Code);
void OnSearchAnswerStart(void);
void OnSearchAnswerItem(const char IP[],const char Path[]);
void OnSearchAnswerEnd(void);


#endif /* _CLIENT_H_ */
