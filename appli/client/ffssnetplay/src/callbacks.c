#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>

#include "callbacks.h"
#include "interface.h"
#include "support.h"
#include "client.h"

gboolean
on_clist1_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  GList *items;
  if(event->type == GDK_2BUTTON_PRESS)
  {
    items = FNP_clist->selection;
    if(items != NULL)
      FNP_CurrentSong = (gint) items->data;
    PlayNextFile(false);
  }
  return FALSE;
}


void
on_button1_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  GtkEntry *entry;
  gchar *query;

  entry = (GtkEntry *) lookup_widget(wnd_main,"entry1");
  if(entry != NULL)
  {
    query = gtk_entry_get_text(entry);
    gtk_clist_clear(FNP_clist);
    FNP_SearchFiles(query);
  }
}


gboolean
on_window1_delete_event                (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
  gtk_main_quit();
  return FALSE;
}

