#include "client.h"
#include "interface.h"
#include "main.h"

/* UDP callbacks */
void OnNewState(long int State,const char IP[],const char Domain[],const char Name[],const char OS[],const char Comment[],const char MasterIP[])
{
  char *States[]={"","ON","OFF","QUIET"};
  GtkCList *clist;
  gchar *strings[6];

  printf("Received a new state (%ld) from %s (%s-%s-%s-%s) using master %s\n",State,IP,Domain,Name,OS,Comment,MasterIP);
  gdk_threads_enter();
  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
/*  if(clist != NULL)
    gtk_clist_freeze(clist);*/
  if(clist != NULL)
  {
    strings[0] = (gchar *)Name;
    strings[1] = (gchar *)Comment;
    strings[2] = (gchar *)States[State];
    strings[3] = (gchar *)IP;
    strings[4] = (gchar *)OS;
    strings[5] = (gchar *)Domain;
    gtk_clist_append(clist,strings);
  }
  gdk_threads_leave();
}

void OnSharesListing(const char IP[],const char **Names,const char **Comments,int NbShares)
{
  int i;
  GtkCList *clist;
  gchar *strings[2];

  gdk_threads_enter();
  clist = (GtkCList *) lookup_widget(wnd_main,"clist3");
  if(clist != NULL)
    gtk_clist_clear(clist);

  for(i=0;i<NbShares;i++)
  {
    if(clist != NULL)
    {
      strings[0] = (gchar *)Names[i];
      strings[1] = (gchar *)Comments[i];
      gtk_clist_append(clist,strings);
    }
  }
  gdk_threads_leave();
}

/* WARNING !! (char *) of the FM_PHost structure are pointers to STATIC buffer, and must be dupped ! */
/* Except for the FM_PHost->IP that is dupped internaly, and if you don't use it, you MUST free it !! */
void OnServerListingAnswer(const char Domain[],int NbHost,SU_PList HostList)
{
  SU_PList Ptr;
  FM_PHost H;
  char *States[]={"","ON","OFF","QUIET"};
  GtkCList *clist;
  gchar *strings[6];

  gdk_threads_enter();
  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
/*  if(clist != NULL)
    gtk_clist_freeze(clist);*/
  Ptr = HostList;
  while(Ptr != NULL)
  {
    H = (FM_PHost) Ptr->Data;
    if(clist != NULL)
    {
      strings[0] = H->Name;
      strings[1] = H->Comment;
      strings[2] = States[H->State];
      strings[3] = H->IP;
      strings[4] = H->OS;
      strings[5] = (gchar *)Domain;
      gtk_clist_append(clist,strings);
    }
    free(H->IP);
    Ptr = Ptr->Next;
  }
  gdk_threads_leave();
}

void OnEndServerListingAnswer(void)
{
/*  GtkCList *clist;

  gdk_threads_enter();
  printf("unlock\n");
  clist = (GtkCList *) lookup_widget(wnd_main,"clist1");
  if(clist != NULL)
    gtk_clist_thaw(clist);
  gdk_threads_leave();*/
}

void OnDomainListingAnswer(const char **Domains,int NbDomains)
{
  int i;
  GtkCList *clist;
  gchar *strings[1];

  gdk_threads_enter();
  clist = (GtkCList *) lookup_widget(wnd_main,"clist2");
  if(clist != NULL)
  {
    gtk_clist_clear(clist);
    strings[0] = "All";
    gtk_clist_append(clist,strings);
  }

  for(i=0;i<NbDomains;i++)
  {
    if(clist != NULL)
    {
      strings[0] = (gchar *)Domains[i];
      gtk_clist_append(clist,strings);
    }
  }
  gdk_threads_leave();
}

void OnMasterSearchAnswer(struct sockaddr_in Master,FFSS_Field MasterVersion,const char Domain[])
{
  printf("Received a MASTER at ip %s using version %ld for domain %s\n",inet_ntoa(Master.sin_addr),MasterVersion,Domain);
  MyMaster = strdup(inet_ntoa(Master.sin_addr));
  if(FC_SendMessage_DomainListing(inet_ntoa(Master.sin_addr)))
    FC_SendMessage_ServerList(inet_ntoa(Master.sin_addr),NULL,NULL);
  else
    FC_SendMessage_ServerSearch();
}

void add_conn_count(PConn Conn)
{
  assert(Conn);
  G_LOCK(gbl_conns_lock);
  Conn->ref_count++;
  G_UNLOCK(gbl_conns_lock);
}

bool remove_conn(PConn Conn,bool clear_wnd)
{
  bool res = false;
  assert(Conn);

  G_LOCK(gbl_conns_lock);
  Conn->ref_count--;
  assert(Conn->ref_count >= 0);
  if(Conn->ref_count == 0)
  {
    /* Remove Conn from gbl_conns, to protect future invalid uses */
    gbl_conns = g_list_remove(gbl_conns,(gpointer)Conn);
    if(Conn->Active)
    {
      FC_SendMessage_Disconnect(Conn->Server);
    }
    if(Conn->host_share != NULL)
      free(Conn->host_share);
    if(Conn->path != NULL)
      free(Conn->path);
    free(Conn);
    res = true;
  }
  else if(clear_wnd)
    Conn->wnd = NULL;
  G_UNLOCK(gbl_conns_lock);
  return res;
}

PConn lookup_conn(SU_PClientSocket Server,bool even_null_wnd)
{
  GList *ptr;
  PConn Conn = NULL;
  //PConn wnd_conn;

  G_LOCK(gbl_conns_lock);
  ptr = gbl_conns;
  while(ptr)
  {
    Conn = (PConn) ptr->data;
    assert(Conn != NULL);
    if(Conn->Active && (Conn->wnd || even_null_wnd))
    {
      /*assert(Conn->wnd != NULL);
      wnd_conn = (PConn) gtk_object_get_user_data(GTK_OBJECT(Conn->wnd));
      assert(wnd_conn != NULL);
      if(wnd_conn->Server == (gpointer)Server)
        break;*/
      if(Conn->Server == (gpointer)Server)
        break;
    }
    Conn = NULL;
    ptr = ptr->next;
  }
  G_UNLOCK(gbl_conns_lock);
  return Conn;
}

/* TCP callbacks */
bool OnError(SU_PClientSocket Server,int Code,const char Descr[])
{
  PConn Conn = lookup_conn(Server,false);

  if(Conn == NULL)
    return false;

  if(Code == FFSS_ERROR_NO_ERROR)
  {
    gdk_threads_enter();
    gtk_statusbar_pop(Conn->sb,Conn->ctx_id);
    gtk_statusbar_push(Conn->sb,Conn->ctx_id,"Successfully connected to server");
    gdk_threads_leave();
/*    switch(NextToDo)
    {
      case NEXT_TO_DO_LISTING :*/
        return FC_SendMessage_DirectoryListing(Server,Conn->path);
    //}
    return true;
  }
  gdk_threads_enter();
  gtk_statusbar_pop(Conn->sb,Conn->ctx_id);
  gtk_statusbar_push(Conn->sb,Conn->ctx_id,Descr);
  gdk_threads_leave();
  return true;
}

void PrintSize(FFSS_LongField Size,char *buf,int buf_size)
{
  char mod[]=" KMGT";
  int pos = 0;
  double val = Size;

  while(val > 1024)
  {
    val = val / 1024;
    pos++;
  }
  if(pos == 0)
    snprintf(buf,buf_size,"%lld",Size);
  else
    snprintf(buf,buf_size,"%.2lf%c",val,mod[pos]);
}

bool OnDirectoryListingAnswer(SU_PClientSocket Server,const char Path[],int NbEntries,SU_PList Entries)
{
  PConn Conn = lookup_conn(Server,false);
  gchar *strings[3];
  FC_PEntry Ent;
  char buf[1024];
  char buf2[1024];
  SU_PList Ptr;

  if(Conn == NULL)
    return false;

  if(Conn->recursif) /* Within recursive listing */
  {
    gdk_threads_enter();
    Ptr = Entries;
    while(Ptr != NULL)
    {
      Ent = (FC_PEntry) Ptr->Data;
      snprintf(buf,sizeof(buf),"%s/%s",Path,Ent->Name);
      snprintf(buf2,sizeof(buf2),"%s%s",Conn->rec_local_path,buf+strlen(Conn->rec_path));
      if(Ent->Flags & FFSS_FILE_DIRECTORY)
      {
        MKDIR(buf2);
        G_LOCK(gbl_conns_lock); /* To protect Conn->rec_dir_count */
        Conn->rec_dir_count++;
        G_UNLOCK(gbl_conns_lock);
        FC_SendMessage_DirectoryListing(Conn->Server,buf);
      }
      else
      {
        char tmp[1024];
        snprintf(tmp,sizeof(tmp),"%ld",Ent->Size);
        add_file_to_download(Conn,buf,buf2,tmp,false);
      }
      Ptr = Ptr->Next;
    }
    G_LOCK(gbl_conns_lock); /* To protect Conn->rec_dir_count */
    Conn->rec_dir_count--;
    G_UNLOCK(gbl_conns_lock);
    assert(Conn->rec_dir_count >= 0);
    if(Conn->rec_dir_count == 0) /* End of recursive directory listing */
    {
      Conn->recursif = false;
      if(Conn->rec_path != NULL)
        free(Conn->rec_path);
      if(Conn->rec_local_path != NULL)
        free(Conn->rec_local_path);
      gtk_statusbar_pop(Conn->sb,Conn->ctx_id);
      gtk_statusbar_push(Conn->sb,Conn->ctx_id,"Recursive download : Listing complete");
    }
    else
    {
      char tmp[1024];
      snprintf(tmp,sizeof(tmp),"Recursive download : Listing directory %s",Path);
      gtk_statusbar_pop(Conn->sb,Conn->ctx_id);
      gtk_statusbar_push(Conn->sb,Conn->ctx_id,tmp);
    }
    gdk_threads_leave();
  }
  else
  {
    gdk_threads_enter();
    gtk_clist_clear(Conn->list);
    free(Conn->path);
    Conn->path = strdup(Path);
    //Form->StatusBar1->SimpleText = "//" + Host + "/" + Share + Path;
    if(Conn->path[1] != 0) /* Not root path */
    {
      strings[0] = "Dir";
      strings[1] = "..";
      strings[2] = "0";
      gtk_clist_append(Conn->list,strings);
    }

    Ptr = Entries;
    while(Ptr != NULL)
    {
      Ent = (FC_PEntry) Ptr->Data;
      if(Ent->Flags & FFSS_FILE_DIRECTORY)
        strings[0] = strdup("Dir");
      else
        strings[0] = "File";
      strings[1] = Ent->Name;
      PrintSize(Ent->Size,buf,sizeof(buf));
      strings[2] = buf;
      gtk_clist_append(Conn->list,strings);
      Ptr = Ptr->Next;
    }
    gdk_threads_leave();
  }
  return true;
}

void OnEndTCPThread(SU_PClientSocket Server)
{
  PConn Conn = lookup_conn(Server,true);

  printf("On End TCP Thread\n");
  if(Conn == NULL)
    return;
  Conn->Active = false;
  gdk_threads_enter();
  gtk_clist_clear(Conn->list);
  gdk_threads_leave();
}

void OnIdleTimeout(SU_PClientSocket Server)
{
  PConn Conn = lookup_conn(Server,true);

  if(Conn == NULL)
    return;
  Conn->Active = false;
}

void launch_next_download(void)
{
  PConn Conn;
  gchar *remote = NULL,*local = NULL;
  FILE *fp;

  while(1)
  {
    Conn = (PConn) gtk_clist_get_row_data(clist_dwl,0);
    if(Conn != NULL)
    {
      if(!Conn->Active) /* Connection closed */
      {
        gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;
        /* Move file to Fail list */
        gtk_clist_get_text(clist_dwl,0,0,&host_share);
        assert(host_share);
        gtk_clist_get_text(clist_dwl,0,1,&remote);
        assert(remote);
        gtk_clist_get_text(clist_dwl,0,2,&local);
        assert(local);
        gtk_clist_get_text(clist_dwl,0,3,&size);
        assert(size);
        move_file_to_failed(host_share,remote,local,size,"Connection with server's share closed");
        /* Remove Xfer from queue */
        gtk_clist_remove(clist_dwl,0);
        remove_conn(Conn,false);
        /* Launch next download */
        continue;
      }
      gtk_clist_get_text(clist_dwl,0,1,&remote);
      assert(remote);
      gtk_clist_get_text(clist_dwl,0,2,&local);
      assert(local);
      fp = fopen(local,"rb");
      if(fp != NULL)
      {
        long int fsize;
        gchar *size = NULL;
        GtkWidget *wnd,*button;
        GtkLabel *label;
        char tmp[1024];

        fseek(fp,0,SEEK_END);
        fsize = ftell(fp);
        fclose(fp);
        gtk_clist_get_text(clist_dwl,0,3,&size);
        assert(size);
        /* Create file action windows */
        wnd = create_window3();
        label = (GtkLabel *) lookup_widget(wnd,"label29");
        assert(label != NULL);
        snprintf(tmp,sizeof(tmp),"File size differs for \"%s\" :\n  Local  : %8ld\n  Remote : %8ld",remote,fsize,atol(size));
        gtk_label_set_text(label,tmp);
        gtk_object_set_user_data(GTK_OBJECT(wnd),(gpointer)Conn);
        if(atol(size) <= fsize) /* Same size or local bigger, overwrite ? */
          button = lookup_widget(wnd,"button8");
        else /* File exists, but not same size... resume ? */
        {
          snprintf(tmp,sizeof(tmp),"%ld",fsize);
          gtk_clist_set_text(clist_dwl,0,4,tmp);
          button = lookup_widget(wnd,"button7");
        }
        gtk_widget_grab_focus(button);
        gtk_widget_show(wnd);
      }
      else
      {
        add_conn_count(Conn);
        FFSS_DownloadFile(Conn->Server,remote,local,0,(void *)Conn,false,NULL);
      }
    }
    break;
  }
}

void add_file_to_download(PConn Conn,char *remote,char *local,char *size,bool lock)
{
  gchar *strings[5];
  gint row;

  if(lock)
    gdk_threads_enter();
  add_conn_count(Conn);

  strings[0] = Conn->host_share;
  strings[1] = remote;
  strings[2] = local;
  strings[3] = size;
  strings[4] = "0";
  row = gtk_clist_append(clist_dwl,strings);
  gtk_clist_set_row_data(clist_dwl,row,(gpointer)Conn);
  if(row == 0) /* No active download, launch it */
    launch_next_download();

  if(lock)
    gdk_threads_leave();
}

void move_file_to_failed(char *host_share,char *remote,char *local,char *size,char *error)
{
  gchar *strings[5];

  strings[0] = host_share;
  strings[1] = remote;
  strings[2] = local;
  strings[3] = size;
  strings[4] = error;
  gtk_clist_append(clist_fail,strings);
}

void OnTransfertFailed(FFSS_PTransfer FT,FFSS_Field ErrorCode,const char Error[],bool Download)
{
  PConn Conn;
  gint row;
  gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;

  gdk_threads_enter();
  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    FT->User = NULL;
    remove_conn(Conn,false);
    row = gtk_clist_find_row_from_data(clist_dwl,(gpointer)Conn);
    assert(row != -1);
    if(row != -1)
    {
      /* Move file to Fail list */
      gtk_clist_get_text(clist_dwl,row,0,&host_share);
      assert(host_share);
      gtk_clist_get_text(clist_dwl,row,1,&remote);
      assert(remote);
      gtk_clist_get_text(clist_dwl,row,2,&local);
      assert(local);
      gtk_clist_get_text(clist_dwl,row,3,&size);
      assert(size);
      move_file_to_failed(host_share,remote,local,size,(char *)Error);
      /* Remove Xfer from queue */
      gtk_clist_remove(clist_dwl,row);
      remove_conn(Conn,false);
      /* Launch next download */
      launch_next_download();
    }
  }
  gdk_threads_leave();
}

void OnTransfertSuccess(FFSS_PTransfer FT,bool Download)
{
  PConn Conn;
  gint row;

  gdk_threads_enter();
  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    FT->User = NULL;
    remove_conn(Conn,false);
    row = gtk_clist_find_row_from_data(clist_dwl,(gpointer)Conn);
    assert(row != -1);
    if(row != -1)
    {
      /* Remove Xfer from queue */
      gtk_clist_remove(clist_dwl,row);
      remove_conn(Conn,false);
      /* Launch next download */
      launch_next_download();
    }
  }
  gdk_threads_leave();
}

void OnTransfertActive(FFSS_PTransfer FT,long int Amount,bool Download)
{
  PConn Conn;
  gint row;
  gchar *position = NULL;
  char buf[50];

  gdk_threads_enter();
  Conn = (PConn)FT->User;
  assert(Conn);
  if(Conn != NULL)
  {
    row = gtk_clist_find_row_from_data(clist_dwl,(gpointer)Conn);
    assert(row != -1);
    if(row != -1)
    {
      /* Update download position */
      gtk_clist_get_text(clist_dwl,row,4,&position);
      assert(position);
      snprintf(buf,sizeof(buf),"%ld",atol(position)+Amount);
      gtk_clist_set_text(clist_dwl,row,4,buf);
    }
  }
  gdk_threads_leave();
}

/*
FFSS_PTransfer OnInitXFer(SU_PClientSocket Server,const char RequestedFileName[])
{
  TConn *Conn;

  Conn = Form1->GetConn(Server);
  if(Conn == NULL)
    return NULL;
  return Conn->OnInitXFer(Server,RequestedFileName);
}

FFSS_PTransfer OnData(SU_PClientSocket Server,FFSS_Field XFerTag)
{
  TConn *Conn;

  Conn = Form1->GetConn(Server);
  if(Conn == NULL)
    return NULL;
  return Conn->OnData(Server,XFerTag);
}
*/
