
#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <gtk/gtk.h>

#include "callbacks.h"
#include "interface.h"
#include "support.h"
#include "client.h"
#include "main.h"
#include "draw.h"

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
  gchar *domain = NULL;
  GtkTreeIter iter;
  GtkTreeModel *store;
  GtkTreeSelection *select;
  GtkTreeView *view;

  store = (GtkTreeModel *) lookup_widget(wnd_main,"store2");
  if(store != NULL)
  {
    gtk_list_store_clear(GTK_LIST_STORE(store));
  }
  store = (GtkTreeModel *) lookup_widget(wnd_main,"store3");
  if(store != NULL)
  {
    gtk_list_store_clear(GTK_LIST_STORE(store));
  }
  store = (GtkTreeModel *) lookup_widget(wnd_main,"store1");
  view = (GtkTreeView *) lookup_widget(wnd_main,"list1");
  if((store != NULL) && (view != NULL))
  {
    select = gtk_tree_view_get_selection(view);
    if(select != NULL)
    {
      if(gtk_tree_selection_get_selected(select,&store,&iter))
        gtk_tree_model_get(store,&iter,0,&domain,-1);
    }
  }
  if(MyMaster != NULL)
  {
    if(FC_SendMessage_DomainListing(MyMaster,0))
      FC_SendMessage_ServerList(MyMaster,NULL,domain,0);
    else
      FC_SendMessage_ServerSearch();
  }
  else
    FC_SendMessage_ServerSearch();
  if(domain != NULL)
    g_free(domain);
}


void
on_button3_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  gchar *server = NULL;
  GtkTreeIter iter;
  GtkTreeModel *store;
  GtkTreeSelection *select;
  GtkTreeView *view;

  store = (GtkTreeModel *) lookup_widget(wnd_main,"store3");
  if(store != NULL)
  {
    gtk_list_store_clear(GTK_LIST_STORE(store));
  }
  store = (GtkTreeModel *) lookup_widget(wnd_main,"store2");
  view = (GtkTreeView *) lookup_widget(wnd_main,"list2");
  if((store != NULL) && (view != NULL))
  {
    select = gtk_tree_view_get_selection(view);
    if(select != NULL)
    {
      if(gtk_tree_selection_get_selected(select,&store,&iter))
        gtk_tree_model_get(store,&iter,3,&server,-1);
    }
  }
  if(server != NULL)
  {
    FC_SendMessage_SharesListing(server,0);
    g_free(server);
  }
}

void
on_button4_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  gchar *server = NULL;
  gchar *share = NULL;
  SU_PClientSocket CS = NULL;
  GtkTreeIter iter;
  GtkTreeModel *store;
  GtkTreeSelection *select;
  GtkTreeView *view;

  store = (GtkTreeModel *) lookup_widget(wnd_main,"store3");
  view = (GtkTreeView *) lookup_widget(wnd_main,"list3");
  if((store != NULL) && (view != NULL))
  {
    select = gtk_tree_view_get_selection(view);
    if(select != NULL)
    {
      if(gtk_tree_selection_get_selected(select,&store,&iter))
        gtk_tree_model_get(store,&iter,0,&share,-1);
      if(share != NULL)
      {
        store = (GtkTreeModel *) lookup_widget(wnd_main,"store2");
        view = (GtkTreeView *) lookup_widget(wnd_main,"list2");
        if((store != NULL) && (view != NULL))
        {
          select = gtk_tree_view_get_selection(view);
          if(select != NULL)
          {
            if(gtk_tree_selection_get_selected(select,&store,&iter))
              gtk_tree_model_get(store,&iter,3,&server,-1);
            if(server != NULL)
            {
              G_LOCK(gbl_conns_lock);
              CS = FC_SendMessage_ShareConnect(server,share,NULL,NULL,0); /* Login/Password set to null right now */
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
                Conn->store = (GtkTreeModel *) lookup_widget(Conn->wnd,"store1");
                assert(Conn->store != NULL);
                Conn->view = (GtkTreeView *) lookup_widget(Conn->wnd,"list1");
                assert(Conn->view != NULL);
                gtk_object_set_user_data(GTK_OBJECT(Conn->wnd),(gpointer)Conn);
                printf("Adding conn to gbl_conns : %p (CS=%p)\n",Conn,CS);
                gbl_conns = g_list_append(gbl_conns,(gpointer)Conn);
                G_UNLOCK(gbl_conns_lock);
                gtk_window_set_title(GTK_WINDOW(Conn->wnd),Conn->host_share);
                gtk_widget_show(Conn->wnd);
              }
              else
                G_UNLOCK(gbl_conns_lock);
              g_free(server);
            }
          }
        }
        g_free(share);
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
on_list3_button_press_event            (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  if(event->type == GDK_2BUTTON_PRESS)
    on_button4_clicked(NULL,NULL);
  return FALSE;
}

void on_list1_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  on_rescan_clicked(NULL,NULL);
}

void on_list2_selection_changed(GtkTreeSelection *selection, gpointer data)
{
  on_button3_clicked(NULL,NULL);
}


gboolean
on_list1_button_press_event            (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data)
{
  PConn Conn;
  gchar *type = NULL;
  gchar *file = NULL;
  GtkTreeIter iter;
  GtkTreeSelection *select;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);

  if(Conn->recursif)
    return FALSE;
  if(event == NULL || event->type == GDK_2BUTTON_PRESS) /* Double clicked */
  {
    select = gtk_tree_view_get_selection(Conn->view);
    if(select != NULL)
    {
      if(gtk_tree_selection_get_selected(select,NULL,&iter))
      {
        gtk_tree_model_get(Conn->store,&iter,0,&type,-1);
        assert(type);
        if(SU_strcasecmp(type,"Dir"))
        { /* If directory, active recurse download, or go into it */
          if(widget == NULL) /* Clicked on download button */
          {
            GtkWidget *filesel;
            filesel = create_fileselection2();
            gtk_object_set_user_data(GTK_OBJECT(filesel),(gpointer)Conn);
            gtk_tree_model_get(Conn->store,&iter,1,&file,-1);
            assert(file);
            gtk_file_selection_set_filename(GTK_FILE_SELECTION(filesel),file);
            gtk_widget_show(filesel);
            if(file != NULL)
              g_free(file);
          }
          else /* Double clicked on dir name, so go into it */
          {
            char buf[1024];
            gtk_tree_model_get(Conn->store,&iter,1,&file,-1);
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
            FC_SendMessage_DirectoryListing(Conn->Server,buf,0);
            if(file != NULL)
              g_free(file);
          }
        }
        else
        { /* If file, download it */
          GtkWidget *filesel;
          filesel = create_fileselection1();
          gtk_object_set_user_data(GTK_OBJECT(filesel),(gpointer)Conn);
          gtk_tree_model_get(Conn->store,&iter,1,&file,-1);
          assert(file);
          gtk_file_selection_set_filename(GTK_FILE_SELECTION(filesel),file);
          gtk_widget_show(filesel);
          if(file != NULL)
            g_free(file);
        }
        if(type != NULL)
          g_free(type);
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
  on_list1_button_press_event(NULL,NULL,user_data);
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
  gchar *type = NULL;
  gchar *file = NULL;
  gchar *size = NULL;
  GtkTreeIter iter;
  GtkTreeSelection *select;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);

  select = gtk_tree_view_get_selection(Conn->view);
  if(select != NULL)
  {
    if(gtk_tree_selection_get_selected(select,NULL,&iter))
    {
      gtk_tree_model_get(Conn->store,&iter,0,&type,-1);
      assert(type);
      if(SU_strcasecmp(type,"Dir"))
      { /* If directory, go in it */
        printf("Shouldn't have DIR row here\n");
        abort();
      }
      else
      { /* If file, download it */
        char buf[1024];

        gtk_tree_model_get(Conn->store,&iter,1,&file,-1);
        assert(file);
        gtk_tree_model_get(Conn->store,&iter,2,&size,-1);
        assert(size);
        if(Conn->path[1] == 0) /* Root path */
          snprintf(buf,sizeof(buf),"/%s",file);
        else
          snprintf(buf,sizeof(buf),"%s/%s",Conn->path,file);
        FC_DQ_AddOpToList_AddFileToDownload(Conn,buf,gtk_file_selection_get_filename(GTK_FILE_SELECTION(user_data)),size);
        if(file != NULL)
          g_free(file);
        if(size != NULL)
          g_free(size);
      }
      if(type != NULL)
        g_free(type);
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
    GtkTreeIter iter;
    GtkTreeSelection *select;
    gchar *type;
    gchar *file;

    select = gtk_tree_view_get_selection(Conn->view);
    if(select != NULL)
    {
      if(gtk_tree_selection_get_selected(select,NULL,&iter))
      {
        gtk_tree_model_get(Conn->store,&iter,0,&type,-1);
        assert(type);
        if(SU_strcasecmp(type,"Dir"))
        {
          gtk_tree_model_get(Conn->store,&iter,1,&file,-1);
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
          FC_SendMessage_DirectoryListing(Conn->Server,Conn->rec_path,0);
          if(file != NULL)
            g_free(file);
        }
        else
        { /* If file, fail */
          printf("Shouldn't have FILE row here\n");
          abort();
        }
        if(type != NULL)
          g_free(type);
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
  GtkTreeIter iter;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  assert(Conn != NULL);
  gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),&iter);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,1,&remote,-1);
  assert(remote);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,2,&local,-1);
  assert(local);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,4,&position,-1);
  assert(position);

  add_conn_count(Conn);
  FFSS_DownloadFile(Conn->Server,remote,local,atoi(position),(void *)Conn,false,0,NULL);
  if(remote != NULL)
    g_free(remote);
  if(local != NULL)
    g_free(local);
  if(position != NULL)
    g_free(position);
  gtk_widget_destroy(user_data);
}


void
on_button8_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Overwrite clicked */
  GtkTreeIter iter;

  gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),&iter);
  gtk_list_store_set(store_dwl,&iter,4,"0",-1);
  on_button7_clicked(button,user_data);
}


void
on_button9_clicked                     (GtkButton       *button,
                                        gpointer         user_data)
{
  /* Ignore clicked */
  gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;
  PConn Conn;
  GtkTreeIter iter;

  Conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(user_data));
  /* Move file to Fail list */
  gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),&iter);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,0,&host_share,-1);
  assert(host_share);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,1,&remote,-1);
  assert(remote);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,2,&local,-1);
  assert(local);
  gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,3,&size,-1);
  assert(size);
  FCQ_MoveFileToFailed(host_share,remote,local,size,"Overwrite ignored");
  /* Remove Xfer from queue */
  gtk_list_store_remove(store_dwl,&iter);
  remove_conn(Conn,false);
  /* Launch next download */
  FCQ_LaunchNextDownload();
  if(host_share != NULL)
    g_free(host_share);
  if(remote != NULL)
    g_free(remote);
  if(local != NULL)
    g_free(local);
  if(size != NULL)
    g_free(size);
  gtk_widget_destroy(user_data);
}

