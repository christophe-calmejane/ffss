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
  if(event->type == GDK_2BUTTON_PRESS)
    FNP_PlayNextFile(false);
  return FALSE;
}


void
on_button1_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  GtkEntry *entry;
  gchar *query;
  char buf[1024];

  entry = (GtkEntry *) lookup_widget(wnd_main,"entry1");
  if(entry != NULL)
  {
    query = gtk_entry_get_text(entry);
    snprintf(buf,sizeof(buf),"%s mp3",query);
    FC_SendMessage_Search(CLT_MASTER,NULL,buf);
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

