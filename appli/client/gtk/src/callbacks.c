
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>

#include "callbacks.h"
#include "interface.h"
#include "support.h"
#include "client.h"
#include "main.h"

gboolean
on_window1_delete_event                (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
  gtk_main_quit();
  return FALSE;
}


void
on_rescan_clicked                      (GtkButton       *button,
                                        gpointer         user_data)
{
  GtkCList *clist;
  char *domain = NULL;
  GList *items;

  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
  if(clist != NULL)
  {
    gtk_clist_clear(clist);
  }
  clist = (GtkCList *) lookup_widget(wnd_main,"clist3");
  if(clist != NULL)
  {
    gtk_clist_clear(clist);
  }
  clist = (GtkCList *) lookup_widget(wnd_main,"clist2");
  if(clist != NULL)
  {
    items = GTK_CLIST(clist)->selection;
    if((items != NULL) && ((gint)items->data != 0))
    {
      gtk_clist_get_text(clist,(gint)items->data,0,&domain);
    }
  }
  /* Sending server request message to broadcast */
#ifdef HAVE_MASTER
  FC_SendMessage_ServerList(CLT_MASTER,NULL,domain);
#else
  FC_SendMessage_ServerSearch();
#endif
}


void
on_button3_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  GtkCList *clist;
  char *server = NULL;
  GList *items;

  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
  if(clist != NULL)
  {
    items = GTK_CLIST(clist)->selection;
    if(items != NULL)
    {
      gtk_clist_get_text(clist,(gint)items->data,3,&server);
    }
  }
  if(server != NULL)
    FC_SendMessage_SharesListing(server);
}

void
on_button4_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  GtkCList *clist;
  char *server = NULL;
  char *share = NULL;
  SU_PClientSocket CS = NULL;
  GList *items;

  clist = (GtkCList *) lookup_widget(wnd_main,"clist3");
  if(clist != NULL)
  {
    items = GTK_CLIST(clist)->selection;
    if(items != NULL)
    {
      gtk_clist_get_text(clist,(gint)items->data,0,&share);
      if(share != NULL)
      {
        clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
        if(clist != NULL)
        {
          items = GTK_CLIST(clist)->selection;
          if(items != NULL)
          {
            gtk_clist_get_text(clist,(gint)items->data,3,&server);
            if(server != NULL)
            {
              G_LOCK(gbl_conns_lock);
              CS = FC_SendMessage_ShareConnect(server,share,NULL,NULL); /* Login/Password set to null right now */
              if(CS != NULL)
              {
                PConn Conn;
                char s[1024];
                /* Create connect window - Adds CS to it */
                Conn = (PConn) malloc(sizeof(TConn));
                memset(Conn,0,sizeof(TConn));
                snprintf(s,sizeof(s),"%s/%s",server,share);
                Conn->host_share = strdup(s);
                Conn->wnd = create_window2();
                Conn->Server = CS;
                Conn->Active = true;
                Conn->path = strdup("/");
                Conn->ref_count = 1; /* One for the window */
                Conn->sb = (GtkStatusbar *) lookup_widget(Conn->wnd,"statusbar1");
                assert(Conn->sb != NULL);
                Conn->ctx_id = gtk_statusbar_get_context_id(Conn->sb,"mycontext");
                Conn->list = (GtkCList *) lookup_widget(Conn->wnd,"clist4");
                assert(Conn->list != NULL);
                gtk_object_set_user_data(GTK_OBJECT(Conn->wnd),(gpointer)Conn);
                printf("Adding conn to gbl_conns : %p (CS=%p)\n",Conn,CS);
                gbl_conns = g_list_append(gbl_conns,(gpointer)Conn);
                G_UNLOCK(gbl_conns_lock);
                gtk_window_set_title(GTK_WINDOW(Conn->wnd),Conn->host_share);
                gtk_widget_show(Conn->wnd);
              }
              else
                G_UNLOCK(gbl_conns_lock);
            }
          }
        }
      }
    }
  }
}


void
on_button2_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Search engine window */
}


gboolean
on_clist2_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  if(event->type == GDK_2BUTTON_PRESS)
    on_rescan_clicked(NULL,NULL);
  return FALSE;
}


gboolean
on_clist1_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  if(event->type == GDK_2BUTTON_PRESS)
    on_button3_clicked(NULL,NULL);
  return FALSE;
}


gboolean
on_clist3_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  if(event->type == GDK_2BUTTON_PRESS)
    on_button4_clicked(NULL,NULL);
  return FALSE;
}


void
on_clist1_unselect_row                 (GtkCList        *clist,
                                        gint             row,
                                        gint             column,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
  GtkCList *list;

  list = (GtkCList *) lookup_widget(wnd_main,"clist3");
  if(list != NULL)
  {
    gtk_clist_clear(list);
  }
}


gboolean
on_clist4_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  PConn Conn;
  GList *items;
  gchar *type = NULL;
  gchar *file = NULL;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);

  if(Conn->recursif)
    return FALSE;
  if(event == NULL || event->type == GDK_2BUTTON_PRESS) /* Double clicked */
  {
    items = Conn->list->selection;
    if(items != NULL)
    {
      gtk_clist_get_text(Conn->list,(gint)items->data,0,&type);
      assert(type);
      if(SU_strcasecmp(type,"Dir"))
      { /* If directory, active recurse download, or go into it */
        if(widget == NULL) /* Clicked on download button */
        {
          GtkWidget *filesel;
          filesel = create_fileselection2();
          gtk_object_set_user_data(GTK_OBJECT(filesel),(gpointer)Conn);
          gtk_clist_get_text(Conn->list,(gint)items->data,1,&file);
          assert(file);
          gtk_file_selection_set_filename(GTK_FILE_SELECTION(filesel),file);
          gtk_widget_show(filesel);
        }
        else /* Double clicked on dir name, so go into it */
        {
          char buf[1024];
          gtk_clist_get_text(Conn->list,(gint)items->data,1,&file);
          assert(file);
          if(strcmp(file,"..") == 0) /* Going back */
          {
            char *p;
            SU_strcpy(buf,Conn->path,sizeof(buf));
            p = strrchr(buf,'/');
            assert(p);
            if(p != NULL)
            {
              if(p == buf) /* Going back to root directory */
                p[1] = 0;
              else
                p[0] = 0;
            }
          }
          else
          {
            if(Conn->path[1] == 0) /* Root directory */
              snprintf(buf,sizeof(buf),"/%s",file);
            else
              snprintf(buf,sizeof(buf),"%s/%s",Conn->path,file);
          }
          printf("Going into %s\n",buf);
          FC_SendMessage_DirectoryListing(Conn->Server,buf);
        }
      }
      else
      { /* If file, download it */
        GtkWidget *filesel;
        filesel = create_fileselection1();
        gtk_object_set_user_data(GTK_OBJECT(filesel),(gpointer)Conn);
        gtk_clist_get_text(Conn->list,(gint)items->data,1,&file);
        assert(file);
        gtk_file_selection_set_filename(GTK_FILE_SELECTION(filesel),file);
        gtk_widget_show(filesel);
      }
    }
  }
  return FALSE;
}


void
on_button5_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Download clicked */
  on_clist4_button_press_event(NULL,NULL,user_data);
}

gboolean
on_window2_delete_event                (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data)
{
  PConn Conn;
  /* Trying to delete window... ask for it */
  /* If yes, disconnect, and free Conn */
  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(widget));
  assert(Conn != NULL);
  remove_conn(Conn,true);
  gtk_widget_destroy(widget);
  return FALSE;
  /* If no, return true */
  return TRUE;
}

void
on_button6_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Call delete_event */
  on_window2_delete_event(GTK_WIDGET(user_data),NULL,NULL);
}




void
on_ok_button1_clicked                  (GtkButton       *button,
                                        gpointer         user_data)
{
  PConn Conn;
  GList *items;
  gchar *type = NULL;
  gchar *file = NULL;
  gchar *size = NULL;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);

  items = Conn->list->selection;
  if(items != NULL)
  {
    gtk_clist_get_text(Conn->list,(gint)items->data,0,&type);
    assert(type);
    if(SU_strcasecmp(type,"Dir"))
    { /* If directory, go in it */
      printf("Shouldn't have DIR row here\n");
      abort();
    }
    else
    { /* If file, download it */
      char buf[1024];

      gtk_clist_get_text(Conn->list,(gint)items->data,1,&file);
      assert(file);
      gtk_clist_get_text(Conn->list,(gint)items->data,2,&size);
      assert(size);
      if(Conn->path[1] == 0) /* Root path */
        snprintf(buf,sizeof(buf),"/%s",file);
      else
        snprintf(buf,sizeof(buf),"%s/%s",Conn->path,file);
      add_file_to_download(Conn,buf,gtk_file_selection_get_filename(GTK_FILE_SELECTION(user_data)),size,false);
    }
  }
  gtk_widget_destroy(user_data);
}


void
on_cancel_button1_clicked              (GtkButton       *button,
                                        gpointer         user_data)
{
  gtk_widget_destroy(user_data);
}



void
on_ok_button2_clicked                  (GtkButton       *button,
                                        gpointer         user_data)
{
  PConn Conn;
  gchar *path;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);

  path = gtk_file_selection_get_filename(GTK_FILE_SELECTION(user_data));
  assert(path);
  if(path != NULL)
  {
    char buf[1024];
    GList *items;
    gchar *type;
    gchar *file;

    items = Conn->list->selection;
    if(items != NULL)
    {
      gtk_clist_get_text(Conn->list,(gint)items->data,0,&type);
      assert(type);
      if(SU_strcasecmp(type,"Dir"))
      {
        gtk_clist_get_text(Conn->list,(gint)items->data,1,&file);
        assert(file);
        if(Conn->path[1] == 0)
          snprintf(buf,sizeof(buf),"/%s",file);
        else
          snprintf(buf,sizeof(buf),"%s/%s",Conn->path,file);
        Conn->rec_path = strdup(buf);
        Conn->rec_local_path = strdup(path);
        Conn->rec_dir_count = 1; /* Waiting at least one rep */
        Conn->recursif = true;
        MKDIR(Conn->rec_local_path);
        printf("Starting recursive download for %s -- Local savine to %s\n",Conn->rec_path,Conn->rec_local_path);
        FC_SendMessage_DirectoryListing(Conn->Server,Conn->rec_path);
      }
      else
      { /* If file, fail */
        printf("Shouldn't have FILE row here\n");
        abort();
      }
    }
  }
  gtk_widget_destroy(user_data);
}


void
on_cancel_button2_clicked              (GtkButton       *button,
                                        gpointer         user_data)
{
  gtk_widget_destroy(user_data);
}


void
on_button7_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Resume clicked */
  PConn Conn;
  gchar *remote = NULL,*local = NULL,*position = NULL;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);
  gtk_clist_get_text(clist_dwl,0,1,&remote);
  assert(remote);
  gtk_clist_get_text(clist_dwl,0,2,&local);
  assert(local);
  gtk_clist_get_text(clist_dwl,0,4,&position);
  assert(position);

  add_conn_count(Conn);
  FFSS_DownloadFile(Conn->Server,remote,local,atoi(position),(void *)Conn,false,NULL);
  gtk_widget_destroy(user_data);
}


void
on_button8_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Overwrite clicked */
  gtk_clist_set_text(clist_dwl,0,4,"0");
  on_button7_clicked(button,user_data);
}


void
on_button9_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Ignore clicked */
  gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;
  PConn Conn;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  /* Move file to Fail list */
  gtk_clist_get_text(clist_dwl,0,0,&host_share);
  assert(host_share);
  gtk_clist_get_text(clist_dwl,0,1,&remote);
  assert(remote);
  gtk_clist_get_text(clist_dwl,0,2,&local);
  assert(local);
  gtk_clist_get_text(clist_dwl,0,3,&size);
  assert(size);
  move_file_to_failed(host_share,remote,local,size,"Overwrite ignored");
  /* Remove Xfer from queue */
  gtk_clist_remove(clist_dwl,0);
  remove_conn(Conn,false);
  /* Launch next download */
  launch_next_download();
  gtk_widget_destroy(user_data);
}

