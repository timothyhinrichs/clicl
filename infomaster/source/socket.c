/*****************************************************************************/

/* Socket communication code */

/*
  Modified 9/28/95 by Don Geddis from original file by Narinder Singh
  Source: "Unix Network Programming", W. Richard Stevens
          Chapter 6: "Berkeley Sockets"
  Updated 10/14/95 for blocking ACCEPT with SELECT for the listen
  Updated 02/22/96 for filename change
*/

/*
 For Franz Common Lisp in IRIX on SGI:
 cc -c -KPIC socket.c
 ld -shared -all -o socket-sgi.so socket.o

 For use with Franz Common Lisp in Solaris it must be compiled as follows:
 /bin/cc -c -fPIC socket.c
 /usr/ccs/bin/ld -G -o socket-sunsol.so socket.o -lc -lucb -lsocket -lnsl -lintl

 For use with Franz Common Lisp in SunOS 4.1 it must be compiled as follows:
 /bin/cc -c socket.c -o socket-sunos.o
*/

/*****************************************************************************/

#define SOCKQUEUELENGTH 5
#define MAX_MESSAGE_SIZE 1024

#include       <stdio.h>
#include       <sys/types.h>
#include       <sys/time.h>
#include       <sys/socket.h>
#include       <netinet/in.h>
#include       <arpa/inet.h>
#include       <netdb.h>
#include       <varargs.h>
#include       <fcntl.h>

/*****************************************************************************/
/* Structures */

extern int errno;

struct a_conn {
  int   sockfd;   /* Socket descriptor of accepted connection */
  char* hostname; /* The name of the host that connected      */
  char* hostnum;  /* The host IP number, as "36.8.0.71"       */
};

/*****************************************************************************/

/* Opens a socket on the current machine at a given port. The
   "portnum" is the number of the socket, e.g., 4010 The function
   returns an integer value of the new socket descriptor which has
   been (successfully) bound to that port number.  This socket
   descriptor is referred to as the listening socket because its only
   job is to listen for requests to connect. */

/*
  Modified 8/28/95 for error handling
  1. No output upon error
  2. Returns positive integer if successful
  3. Returns negative integer if error:
       -1 => Can't open stream socket
       -2 => Can't set local address reuse socket option
       -3 => Can't bind local address
       -4 => Can't listen to socket
*/

opensocket(portnum)
     int portnum;
{
  int			sockfd, error;
  struct sockaddr_in	serv_addr;
  int                     optval = 1;
  
  error = 0;

  /* Open a TCP socket (an Internet stream socket). */
  if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    error = -1;
  
  /* Set the options for the socket */
  else if ((error =
	    setsockopt(sockfd,SOL_SOCKET,SO_REUSEADDR,(char *)&optval,
		       sizeof(optval) )) < 0)
    error = -2;
  
  /* Bind our local address */
  else {
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family      = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port        = htons((u_short)portnum);
    
    if ((error = bind(sockfd, (struct sockaddr *) &serv_addr,
		      sizeof(serv_addr) )) < 0)
      error = -3;
    
    /* Indicate server is willing to listen to connections */
    else if ((error = listen(sockfd, SOCKQUEUELENGTH)) < 0)
      error = -4;
  }
  
  if (error < 0) return(error);
  else return(sockfd);

} /* opensocket */

/*****************************************************************************/

/* Close a previously opened socket on the current machine.  The
   "sockfd" is a socket descriptor of a successfully opened socket.
   Return a positive integer if success. */

closesocket(sockfd)
int sockfd;
{
  int ans;
  ans = close(sockfd);
  return(ans);
} /* closesocket */

/*****************************************************************************/

/* Open a connection to a given host at a given port.  connectsocket
   has parameters indicating hostname and portnumber to which
   connection is to take place.  connectsocket opens a new socket and
   connects this new socket to the specified host and port.
   connectsocket returns 0 upon error and otherwise returns the socket
   descriptor (integer) that identifies this particular connection.
   Note that the connection is not symmetrically established until the
   receiving party executes an accept */

/*
  Modified error handling 9/28/95:
  1. Upon error, nothing is printed to STDIO
  2. Returned socket > 0 => successful open connection
  3. Otherwise, socket < 0 => error:
        -1 => Unknown host (couldn't get IP address)
	-2 => Couldn't open TCP socket
	-3 => Server not responding
*/


connectsocket(host,portnum)
     char *host;
     int portnum;
{
	int			sockfd;
	struct sockaddr_in	serv_addr;
	struct hostent          *hp;
	
	/* Get the IP number of the host from its name */
	if ( (hp = gethostbyname(host)) == NULL)
	  sockfd = -1;
	else {

	  /* Fill in the structure "serv_addr" with the address of the
	     server that we want to connect with. */

	bzero((char *) &serv_addr, sizeof(serv_addr));
	serv_addr.sin_family = AF_INET;
	bcopy(hp->h_addr, (char *)&serv_addr.sin_addr, hp->h_length);
	serv_addr.sin_port = htons((u_short)portnum);

	/* Open a TCP socket (an Internet stream socket). */
	if ( (sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	  sockfd = -2;
	else {

	  /* Connect to the server. */
	  if (connect(sockfd, (struct sockaddr *) &serv_addr,
		      sizeof(serv_addr) ) < 0)
	    { close(sockfd) ;
	      sockfd = -3; }}}

	return(sockfd);
} /* connectsocket */

/*****************************************************************************/

/* Accept a connection, blocking until one is found.
   The first argument is the socket file descriptor (e.g., 7)
   The second argument is a structure that we fill in:
    o sockfd   - the socket file descriptor of the new socket
    o hostnum  - the IP number of the peer that connected
    o hostname - the host name of the peer that connected
    If you don't want to block, then call the function check_socket
    instead, which returns > 0 if someone is trying to connect to
    this socket, and it returns 0 if no one is waiting to connect */

/*
  Modified.  No longer returns anything, but just sets the socket in the
  structure.  Negative socket values indicate an error:
    -1 => Could not accept a connection
*/

void blockaccept(lsock,new_conn)
int                lsock;
struct a_conn*     new_conn;
{
int                newsock, clilen;
struct sockaddr_in cli_addr;
struct hostent     *hoststr;
static char*       unknownh = "unknown.host.addr";

clilen = sizeof(cli_addr);
newsock = accept(lsock,(struct sockaddr *) &cli_addr,(int *) &clilen);

if (newsock < 0)
  newsock = -1;
else {
  getpeername(lsock,(struct sockaddr *) &cli_addr, (int *) &clilen);
  new_conn->hostnum  = inet_ntoa(cli_addr.sin_addr);
  hoststr =
    gethostbyaddr(&(cli_addr.sin_addr), sizeof(struct in_addr), AF_INET);
  if (hoststr)
    new_conn->hostname = hoststr->h_name;
  else new_conn->hostname = unknownh;
}

new_conn->sockfd   = newsock;
}

/*****************************************************************************/

/* Check if someone is trying to connect to a socket.
   Assuming that "bind" and "listen" already done on the socket.
   Return 0 if no one is trying to connect to the socket,
   and otherwise return 2^<socket-number>
   Return -1 if there was some error in checking the socket */

int check_socket(lsock)
int lsock;

{ 
fd_set fdvar;          /* a variable holding a set of file descriptors */
static struct timeval timeout;

timeout.tv_sec = 0;    /* do not wait when checking socket status */
timeout.tv_usec = 0;

FD_ZERO(&fdvar);       /* initialize the file descriptor set to all zeros */
FD_SET(lsock, &fdvar); /* set the file descriptor set for
			  passed-in descriptor */

if (select (32, &fdvar, (fd_set *) 0, (fd_set *) 0, &timeout) < 0)
   return(-1);         /* -1 indicates some error in checking socket status */
else return(FD_ISSET(lsock,&fdvar));
}

/*****************************************************************************/
