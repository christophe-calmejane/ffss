
#include "confapi.h"

#define FFSSWHO_VERSION "1.0-pre4"

bool RequestConns(SU_PClientSocket Client,const char Path[])
{
  SU_PList Conns,Ptr,Ptr2;
  FSCA_PConn Conn;

  Conns = FSCA_RequestConns(Client,Path);
  if(Conns == NULL)
  {
    printf("No connection\n");
    return true;
  }
  printf("%-15s  %-35s  Tranfers  Streamings\n","IP","Host");
  printf("--------------------------------------------------------------------------\n");
  Ptr = Conns;
  while(Ptr != NULL)
  {
    Conn = (FSCA_PConn)Ptr->Data;
    printf("%-15s  %-35s  %-8d  %-3d\n",Conn->IP,SU_NameOfPort(Conn->IP),Conn->NbXfers,Conn->NbStrms);
    if(Conn->Xfers != NULL)
    {
      printf("  Current tranfers : \n");
      Ptr2 = Conn->Xfers;
      while(Ptr2 != NULL)
      {
        printf("    %s : %s%%\n",((FSCA_PFileInfo)Ptr2->Data)->Name,((FSCA_PFileInfo)Ptr2->Data)->Pct);
        Ptr2 = Ptr2->Next;
      }
    }
    if(Conn->Strms != NULL)
    {
      printf("  Current streamings : \n");
      Ptr2 = Conn->Strms;
      while(Ptr2 != NULL)
      {
        printf("    %s : %s%%\n",((FSCA_PFileInfo)Ptr2->Data)->Name,((FSCA_PFileInfo)Ptr2->Data)->Pct);
        Ptr2 = Ptr2->Next;
      }
    }
    if((Conn->Xfers != NULL) || (Conn->Strms != NULL))
      printf("\n");
    Ptr = Ptr->Next;
  }
  printf("\n");
  return true;
}

bool RequestSharesList(SU_PClientSocket Client)
{
  SU_PList Shares,Ptr;
  FSCA_PShareLst Share;

  Shares = FSCA_RequestSharesList(Client);
  if(Shares == NULL)
    return true;
  Ptr = Shares;
  while(Ptr != NULL)
  {
    Share = (FSCA_PShareLst)Ptr->Data;
    if((Share->NbConns+Share->NbXfers) != 0)
    {
      printf("Connections for share \"%s\" : \n",Share->Name);
      if(!RequestConns(Client,Share->Path))
        return false;
    }
    Ptr = Ptr->Next;
  }
  return true;
}


int main(int argc,char *argv[])
{
  SU_PClientSocket Client;
  char *Server;

#ifdef _WIN32
  if(!SU_WSInit(2,2))
  {
    printf("Cannot initialize winsock2 !\n");
    return -1;
  }
#endif /* _WIN32 */
  printf("FFSS Who v%s (c) Ze KiLleR / SkyTech 2001'02\n",FFSSWHO_VERSION);
  printf("FFSS Server v%s (c) Ze KiLleR / SkyTech 2001'02\n\n",FFSS_SERVER_VERSION);

  Server = "localhost";
  Client = SU_ClientConnect(Server,FFSS_SERVER_CONF_PORT_S,SOCK_STREAM);
  if(Client == NULL)
  {
    printf("Cannot connect to %s:%s\n",Server,FFSS_SERVER_CONF_PORT_S);
    return -2;
  }

  if(argc != 1)
  {
    FSCA_PGlobal Gbl = FSCA_RequestGlobalInfo(Client);
    if(Gbl != NULL)
    {
      printf("Global infos :\n");
      printf("\tName           = %s\n",Gbl->Name);
      printf("\tComment        = %s\n",Gbl->Comment);
      printf("\tMaster         = %s\n",Gbl->Master);
      printf("\tIdle           = %d\n",Gbl->Idle);
      printf("\tMaxConn        = %d\n",Gbl->MaxConn);
      printf("\tMaxXFerPerConn = %d\n",Gbl->MaxXFerPerConn);
      printf("\tFTP            = %d\n",Gbl->FTP);
      printf("\tFTP_MaxConn    = %d\n",Gbl->FTP_MaxConn);
    }
  }
  if(RequestSharesList(Client) == false)
  {
    printf("Cannot request shares for %s\n",Server);
    return -3;
  }

  SU_FreeCS(Client);
  return 0;
}
