#include "client.h"
#include "interface.h"
#include "main.h"
#include "draw.h"

SU_SEM_HANDLE FCQ_Sem;
SU_PList FCQ_Hosts = NULL; /* FCQ_PHost */
SU_PList FCQ_Domains = NULL; /* char * */
SU_PList FCQ_Shares = NULL; /* FCQ_PShare */
SU_PList FCQ_Ops = NULL; /* FCQ_POperation */

/* ****************** */
void FC_FreeHost(FCQ_PHost H)
{
  if(H->Name != NULL)
    free(H->Name);
  if(H->OS != NULL)
    free(H->OS);
  if(H->Comment != NULL)
    free(H->Comment);
  if(H->IP != NULL)
    free(H->IP);
  if(H->Domain != NULL)
    free(H->Domain);
  free(H);
}

void FC_FreeHostList()
{
  SU_PList Ptr;

  Ptr = FCQ_Hosts;
  while(Ptr != NULL)
  {
    FC_FreeHost(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FCQ_Hosts);
  FCQ_Hosts = NULL;
}

void FC_FreeDomainList()
{
  SU_FreeListElem(FCQ_Domains);
  FCQ_Domains = NULL;
}

void FC_FreeShare(FCQ_PShare S)
{
  if(S->Name != NULL)
    free(S->Name);
  if(S->Comment != NULL)
    free(S->Comment);
  free(S);
}

void FC_FreeShareList()
{
  SU_PList Ptr;

  Ptr = FCQ_Shares;
  while(Ptr != NULL)
  {
    FC_FreeShare(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FCQ_Shares);
  FCQ_Shares = NULL;
}

void FC_FreeEntry(FC_PEntry Ent)
{
  if(Ent->Name != NULL)
    free(Ent->Name);
  free(Ent);
}

void FC_FreeOp(FCQ_POperation Op)
{
  SU_PList Ptr;
  int i;

  for(i=0;i<FCQ_MAX_STRINGS;i++)
  {
    if(Op->Strings[i] != NULL)
      free(Op->Strings[i]);
  }
  if(Op->Entries != NULL)
  {
    Ptr = Op->Entries;
    while(Ptr != NULL)
    {
      FC_FreeEntry(Ptr->Data);
      Ptr = Ptr->Next;
    }
    SU_FreeList(Op->Entries);
  }
  free(Op);
}

void FC_FreeOpList()
{
  SU_PList Ptr;

  Ptr = FCQ_Ops;
  while(Ptr != NULL)
  {
    FC_FreeOp(Ptr->Data);
    Ptr = Ptr->Next;
  }
  SU_FreeList(FCQ_Ops);
  FCQ_Ops = NULL;
}


/* ********* UDP ********* */

void FCQ_DrawServers()
{
  SU_PList Ptr;
  FCQ_PHost H;
  char *States[]={"","ON","OFF","QUIET"};
  GtkListStore *store;
  GtkTreeIter iter;
  gchar *s;
  gsize tmp;

  store = (GtkListStore *) lookup_widget(wnd_main,"store2");
  if(store != NULL)
  {
    Ptr = FCQ_Hosts;
    while(Ptr != NULL)
    {
      H = (FCQ_PHost) Ptr->Data;
      gtk_list_store_append(store,&iter);
      s = g_locale_to_utf8(H->Name,-1,NULL,&tmp,NULL);
      if(s == NULL)
        gtk_list_store_set(store,&iter,0,H->Name,-1);
      else
      {
        gtk_list_store_set(store,&iter,0,s,-1);
        g_free(s);
      }
      s = g_locale_to_utf8(H->Comment,-1,NULL,&tmp,NULL);
      if(s == NULL)
        gtk_list_store_set(store,&iter,1,H->Comment,-1);
      else
      {
        gtk_list_store_set(store,&iter,1,s,-1);
        g_free(s);
      }
      gtk_list_store_set(store,&iter,2,States[H->State],3,H->IP,4,H->OS,5,H->Domain,-1);
      Ptr = Ptr->Next;
    }
  }
  FC_FreeHostList();
}

void FCQ_DrawDomains()
{
  SU_PList Ptr;
  GtkListStore *store;
  GtkTreeIter iter;

  store = (GtkListStore *) lookup_widget(wnd_main,"store1");
  if(store != NULL)
  {
    gtk_list_store_clear(store);
    gtk_list_store_append(store,&iter);
    gtk_list_store_set(store,&iter,0,"All",-1);

    Ptr = FCQ_Domains;
    while(Ptr != NULL)
    {
      gtk_list_store_append(store,&iter);
      gtk_list_store_set(store,&iter,0,Ptr->Data,-1);
      Ptr = Ptr->Next;
    }
  }

  FC_FreeDomainList();
}

void FCQ_DrawShares()
{
  SU_PList Ptr;
  FCQ_PShare S;
  GtkListStore *store;
  GtkTreeIter iter;
  gchar *s;
  gsize tmp;

  store = (GtkListStore *) lookup_widget(wnd_main,"store3");
  if(store != NULL)
  {
    gtk_list_store_clear(store);

    Ptr = FCQ_Shares;
    while(Ptr != NULL)
    {
      S = (FCQ_PShare) Ptr->Data;
      gtk_list_store_append(store,&iter);
      s = g_locale_to_utf8(S->Name,-1,NULL,&tmp,NULL);
      if(s == NULL)
        gtk_list_store_set(store,&iter,0,S->Name,-1);
      else
      {
        gtk_list_store_set(store,&iter,0,s,-1);
        g_free(s);
      }
      s = g_locale_to_utf8(S->Comment,-1,NULL,&tmp,NULL);
      if(s == NULL)
        gtk_list_store_set(store,&iter,1,S->Comment,-1);
      else
      {
        gtk_list_store_set(store,&iter,1,s,-1);
        g_free(s);
      }
      Ptr = Ptr->Next;
    }
  }
  FC_FreeShareList();
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

void FCQ_MoveFileToFailed(char *host_share,char *remote,char *local,char *size,char *error)
{
  GtkTreeIter iter;

  gtk_list_store_append(store_fail,&iter);
  gtk_list_store_set(store_fail,&iter,0,host_share,1,remote,2,local,3,size,4,error,-1);
}

void FCQ_LaunchNextDownload(void)
{
  PConn Conn;
  gchar *remote = NULL,*local = NULL;
  FILE *fp;
  GtkTreeIter iter;

  while(1)
  {
    if(!gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),&iter))
      break;
    gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,5,&Conn,-1);
    if(Conn != NULL)
    {
      if(!Conn->Active) /* Connection closed */
      {
        gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;
        /* Move file to Fail list */
        gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,0,&host_share,-1);
        assert(host_share);
        gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,1,&remote,-1);
        assert(remote);
        gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,2,&local,-1);
        assert(local);
        gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,3,&size,-1);
        assert(size);
        FCQ_MoveFileToFailed(host_share,remote,local,size,"Connection with server's share closed");
        /* Remove Xfer from queue */
        gtk_list_store_remove(store_dwl,&iter);
        remove_conn(Conn,false);
        if(host_share != NULL)
          g_free(host_share);
        if(remote != NULL)
          g_free(remote);
        if(local != NULL)
          g_free(local);
        if(size != NULL)
          g_free(size);
        /* Launch next download */
        continue;
      }
      gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,1,&remote,-1);
      assert(remote);
      gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,2,&local,-1);
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
        gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,3,&size,-1);
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
          gtk_list_store_set(store_dwl,&iter,4,tmp,-1);
          button = lookup_widget(wnd,"button7");
        }
        gtk_widget_grab_focus(button);
        gtk_widget_show(wnd);
        if(size != NULL)
          g_free(size);
      }
      else
      {
        add_conn_count(Conn);
        FFSS_DownloadFile(Conn->Server,remote,local,0,(void *)Conn,false,NULL);
      }
      if(remote != NULL)
        g_free(remote);
      if(local != NULL)
        g_free(local);
    }
    break;
  }
}

gboolean FCQ_FindIterFromData(gpointer ptr,GtkTreeIter *iter)
{
  gboolean valid;
  gpointer val;

  valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),iter);
  while(valid)
  {
    gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),iter,5,&val,-1);
    if(val == ptr)
      return TRUE;
    valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(store_dwl),iter);
  }
  return FALSE;
}

void FCQ_DrawOps()
{
  SU_PList Ptr,Ptr2;
  FCQ_POperation Op;
  FC_PEntry Ent;
  char buf[1024];
  GtkTreeIter iter;
  gchar *s;
  gsize tmp;

  Ptr = FCQ_Ops;
  while(Ptr != NULL)
  {
    Op = (FCQ_POperation) Ptr->Data;
    if(Op->Conn != NULL)
    {
      switch(Op->Type)
      {
        case FCQ_OP_STATUS :
          gtk_statusbar_pop(Op->Conn->sb,Op->Conn->ctx_id);
          gtk_statusbar_push(Op->Conn->sb,Op->Conn->ctx_id,Op->Strings[0]);
          break;
        case FCQ_OP_DIRLIST :
          gtk_list_store_clear(GTK_LIST_STORE(Op->Conn->store));
          if(Op->Conn->path[1] != 0) /* Not root path */
          {
            gtk_list_store_append(GTK_LIST_STORE(Op->Conn->store),&iter);
            gtk_list_store_set(GTK_LIST_STORE(Op->Conn->store),&iter,0,"Dir",1,"..",2,"0",-1);
          }

          Ptr2 = Op->Entries;
          while(Ptr2 != NULL)
          {
            Ent = (FC_PEntry) Ptr2->Data;
            gtk_list_store_append(GTK_LIST_STORE(Op->Conn->store),&iter);
            if(Ent->Flags & FFSS_FILE_DIRECTORY)
              gtk_list_store_set(GTK_LIST_STORE(Op->Conn->store),&iter,0,"Dir",-1);
            else
              gtk_list_store_set(GTK_LIST_STORE(Op->Conn->store),&iter,0,"File",-1);
            PrintSize(Ent->Size,buf,sizeof(buf));
            s = g_locale_to_utf8(Ent->Name,-1,NULL,&tmp,NULL);
            if(s == NULL)
              gtk_list_store_set(GTK_LIST_STORE(Op->Conn->store),&iter,1,Ent->Name,2,buf,-1);
            else
            {
              gtk_list_store_set(GTK_LIST_STORE(Op->Conn->store),&iter,1,s,2,buf,-1);
              g_free(s);
            }
            Ptr2 = Ptr2->Next;
          }
          break;
        case FCQ_OP_ADDFILEDWL :
        {
          GtkTreeIter iter2;

          gtk_list_store_append(store_dwl,&iter);
          gtk_tree_model_get_iter_first(GTK_TREE_MODEL(store_dwl),&iter2);
          gtk_list_store_set(store_dwl,&iter,0,Op->Strings[0],1,Op->Strings[1],2,Op->Strings[2],3,Op->Strings[3],4,Op->Strings[4],5,(gpointer)Op->Conn,-1);
          if(iter.user_data == iter2.user_data) /* No active download, launch it */
            FCQ_LaunchNextDownload();
          break;
        }
        case FCQ_OP_ENDTHREAD :
          gtk_list_store_clear(GTK_LIST_STORE(Op->Conn->store));
          break;
        case FCQ_OP_XFERFAILED :
        {
          gchar *host_share = NULL,*remote = NULL,*local = NULL,*size = NULL;

          if(FCQ_FindIterFromData((gpointer)Op->Conn,&iter))
          {
            /* Move file to Fail list */
            gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,0,&host_share,-1);
            assert(host_share);
            gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,1,&remote,-1);
            assert(remote);
            gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,2,&local,-1);
            assert(local);
            gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,3,&size,-1);
            assert(size);
            FCQ_MoveFileToFailed(host_share,remote,local,size,Op->Strings[0]);
            /* Remove Xfer from queue */
            gtk_list_store_remove(store_dwl,&iter);
            if(host_share != NULL)
              g_free(host_share);
            if(remote != NULL)
              g_free(remote);
            if(local != NULL)
              g_free(local);
            if(size != NULL)
              g_free(size);
            remove_conn(Op->Conn,false);
            /* Launch next download */
            FCQ_LaunchNextDownload();
          }
          break;
        }
        case FCQ_OP_XFERSUCCESS :
          if(FCQ_FindIterFromData((gpointer)Op->Conn,&iter))
          {
            /* Remove Xfer from queue */
            gtk_list_store_remove(store_dwl,&iter);
            remove_conn(Op->Conn,false);
            /* Launch next download */
            FCQ_LaunchNextDownload();
          }
          break;
        case FCQ_OP_XFERACTIVE :
        {
          gchar *position = NULL;

          if(FCQ_FindIterFromData((gpointer)Op->Conn,&iter))
          {
            /* Update download position */
            gtk_tree_model_get(GTK_TREE_MODEL(store_dwl),&iter,4,&position,-1);
            assert(position);
            snprintf(buf,sizeof(buf),"%ld",atol(position)+Op->Amount);
            gtk_list_store_set(store_dwl,&iter,4,buf,-1);
            if(position != NULL)
              g_free(position);
          }
          break;
        }
      }
    }
    Ptr = Ptr->Next;
  }
  FC_FreeOpList();
}

/* ********* IDLE ********* */

gboolean FCQ_IdleFunc(gpointer data)
{
  SU_SEM_WAIT(FCQ_Sem);
  if(FCQ_Domains != NULL)
    FCQ_DrawDomains();
  if(FCQ_Hosts != NULL)
    FCQ_DrawServers();
  if(FCQ_Shares != NULL)
    FCQ_DrawShares();
  if(FCQ_Ops != NULL)
    FCQ_DrawOps();
  SU_SEM_POST(FCQ_Sem);

  return TRUE;
}
