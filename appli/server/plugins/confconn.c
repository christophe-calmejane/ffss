/* This is the conf conn plugin for ffss server */
/* (c) Christophe Calmejane - 2001              */
/*     aka Ze KiLleR / SkyTech                  */
/*                                              */
/* http://zekiller.skytech.org                  */
/* mailto : zekiller@skytech.org                */

#define CONFCONN_NAME      "Conf Conn Plugin"
#define CONFCONN_VERSION   "0.1"
#define CONFCONN_COPYRIGHT "(c) Ze KiLleR - 2001"

/* The only file we need to include is server.h */
#include "../src/server.h"
#undef malloc
#undef strdup

/* We have to declare a FS_PPlugin structure for our callbacks */
FS_PPlugin Pl;

/* We declare a callback for the OnCheckConfConn callback */
bool OnCheckConfConn(SU_PClientSocket Client)
{
  printf("In plugin ConfConn\n");
  return true;
}


/* This is the Init fonction (Name it CAREFULLY) called on each LoadPlugin call */
FS_PPlugin Plugin_Init(void)
{
  char Name[1024];

  /* Setting all callbacks to NULL */
  Pl = (FS_PPlugin) malloc(sizeof(FS_TPlugin));
  if(Pl == NULL)
    return NULL;
  memset(Pl,0,sizeof(FS_TPlugin));

  /* Setting plugin name */
  snprintf(Name,sizeof(Name),"%s v%s - %s",CONFCONN_NAME,CONFCONN_VERSION,CONFCONN_COPYRIGHT);
  Pl->Name = strdup(Name);

  /* Setting our callbacks */
  Pl->OnCheckConfConn = OnCheckConfConn;


  /* And finaly returning the FS_PPlugin structure to the server.
   * If something goes wrong during this init function, free everything you have allocated and return NULL.
   * UnInit function will not be called in this case.
  */
  return Pl;
}

/* This is the UnInit fonction (Name it CAREFULLY) called on each UnLoadPlugin call */
void Plugin_UnInit(void)
{
  /* Nothing to do, as Pl->Name is freed after */
}

