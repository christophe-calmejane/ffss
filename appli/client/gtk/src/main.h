#ifndef _MAIN_H_
#define _MAIN_H_

#include "support.h"


extern GtkWidget *wnd_main;
extern GtkListStore *store_dwl,*store_fail;

G_LOCK_EXTERN(gbl_conns_lock);
extern GList *gbl_conns;

#endif /* _MAIN_H_ */
