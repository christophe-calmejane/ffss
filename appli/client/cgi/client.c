#include <ffss.h>

#define CLT_MASTER "ffss.fleming.u-psud.fr"

static int Printing = false;

/* UDP callbacks */
void OnSearchAnswer(const char Query[],const char Domain[],const char **Answers,char **IPs,int NbAnswers)
{
  int i = 0;
  char *col;
  char *States[]={"Unkwn","On","Off","","Quiet"};
  char *Color;

  if(Printing)
  {
    if(NbAnswers == 0)
      printf("<FONT COLOR=Red>No answers for domain %s</FONT>\n",Domain);
    else
    {
      printf("<TABLE WIDTH=\"95%%\"CELLSPACING=0 BORDER=2 BGCOLOR=#EEEEEE><TR><TD bgcolor=\"#D3DCE3\"><FONT COLOR=#2020C0>State</FONT></TD><TD bgcolor=\"#D3DCE3\"><FONT COLOR=Red>Found %d answers for domain %s</FONT></TD></TR><TR></TR>\n",NbAnswers,Domain);
      for(i=0;i<NbAnswers;i++)
      {
        if(i % 2)
          col = "#E0EEEE";
        else
          col = "#EEEEE0";
        switch(Answers[i][0] & FFSS_STATE_ALL)
        {
          case FFSS_STATE_ON :
            Color = "#404040";
            break;
          case FFSS_STATE_OFF :
            Color = "#C0C0C0";
            break;
          case FFSS_STATE_QUIET :
            Color = "#808080";
            break;
          default :
            Color = "#808080";
        }
        printf("<TR><TD BGCOLOR=\"%s\"><FONT SIZE=2 COLOR=%s>&nbsp;%s&nbsp;</FONT></TD>\n",col,Color,States[Answers[i][0] & FFSS_STATE_ALL]);
        printf("<TD BGCOLOR=\"%s\"><FONT SIZE=2 COLOR=%s>&nbsp;%s&nbsp;</FONT></TD></TR>\n",col,Color,&Answers[i][1]);
        free(IPs[i]);
      }
      printf("</TABLE>\n");
    }
    printf("<BR><BR>\n");
  }
}

void OnMasterError(int Code,const char Descr[])
{
  printf("Search returned an error : <FONT SIZE+2 COLOR=Red>%s</FONT><BR>\n",Descr);
  /* Shutting down server */
  FC_UnInit();
  exit(0);
}

/* Fatal error, must shutdown */
void OnUDPError()
{
  printf("Fatal error while trying to reach ffss master. Aborting<BR>\n");
  /* Shutting down server */
  FC_UnInit();
  exit(0);
}

int main(int argc,char *argv[])
{
  printf("Content-Type: text/html\n\n");
  if(argc != 2)
  {
    printf("Error : must have exactly one parameter<BR>\n");
    return 0;
  }

  memset(&FFSS_CB,0,sizeof(FFSS_CB));
  FFSS_CB.CCB.OnSearchAnswer = OnSearchAnswer;
  FFSS_CB.CCB.OnMasterError = OnMasterError;
  FFSS_CB.CCB.OnUDPError = OnUDPError;

  if(!FC_Init())
    return 0;

  /* Sending search message to master */
  printf("<HTML><HEAD><TITLE>Search result for \"%s\"</TITLE></HEAD><BODY BGCOLOR=#EEEEEE><FONT FACE=\"Verdana\"><DIV ALIGN=\"CENTER\"><BR>",argv[1]);
  Printing = true;
  FC_SendMessage_Search(CLT_MASTER,NULL,argv[1]);
  sleep(3);
  Printing = false;
  printf("</DIV></FONT></BODY></HTML>");

  /* Shutting down server */
  FC_UnInit();
  return 0;
}
