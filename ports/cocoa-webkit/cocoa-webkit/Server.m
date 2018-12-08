//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"

#define WELCOME_MSG  0
#define ECHO_MSG     1
#define READ_TIMEOUT 15.0

@implementation Server

- (id) init {
    self = [super init];
    if (self != nil) {
        socketQueue = dispatch_queue_create("socketQueue", NULL);
        listenSocket = [[GCDAsyncSocket alloc]
                        initWithDelegate:self delegateQueue:socketQueue];
        connectedSockets = [[NSMutableArray alloc] initWithCapacity:1];
        isRunning = NO;
    }
    return self;
}

-(void) start {
    NSError *error = nil;
    if(![listenSocket acceptOnPort:8082 error:&error])
    {
        NSLog(@"Error starting server: %@", error);
        return;
    }
    NSLog(@"Echo server started on port %d", [listenSocket localPort]);
    isRunning = YES;
}

-(void) stop {
    [listenSocket disconnect];
    @synchronized(connectedSockets)
    {
        NSUInteger i;
        for (i = 0; i < [connectedSockets count]; i++)
        {
            // Call disconnect on the socket,
            // which will invoke the socketDidDisconnect: method,
            // which will remove the socket from the list.
            [[connectedSockets objectAtIndex:i] disconnect];
        }
    }
    NSLog(@"Stopped Echo server");
    isRunning = NO;
}

- (void)socket:(GCDAsyncSocket *)sock didAcceptNewSocket:(GCDAsyncSocket *)newSocket
{
    @synchronized(connectedSockets)
    {
        [connectedSockets addObject:newSocket];
    }
    
    NSString *host = [newSocket connectedHost];
    UInt16 port = [newSocket connectedPort];
    
    dispatch_async(dispatch_get_main_queue(), ^{
        NSLog(@"Accepted client %@:%d", host, port);
    });
    
    NSString *welcomeMsg = @"Welcome to the AsyncSocket Echo Server\r\n";
    NSData *welcomeData = [welcomeMsg dataUsingEncoding:NSUTF8StringEncoding];
    
    [newSocket writeData:welcomeData withTimeout:-1 tag:WELCOME_MSG];
    [newSocket readDataToData:[GCDAsyncSocket CRLFData] withTimeout:READ_TIMEOUT tag:0];
}

- (void)socket:(GCDAsyncSocket *)sock didWriteDataWithTag:(long)tag
{
    if (tag == ECHO_MSG)
    {
        [sock readDataToData:[GCDAsyncSocket CRLFData] withTimeout:READ_TIMEOUT tag:0];
    }
}

- (void)socket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag
{
    dispatch_async(dispatch_get_main_queue(), ^{
        NSData *strData = [data subdataWithRange:NSMakeRange(0, [data length] - 2)];
        NSString *msg = [[NSString alloc] initWithData:strData encoding:NSUTF8StringEncoding];
        if (msg) {
            NSLog(@"%@", msg);
        }
        else {
            NSLog(@"Error converting received data into UTF-8 String");
        }
    });
    
    // Echo message back to client
    [sock writeData:data withTimeout:-1 tag:ECHO_MSG];
}

- (void)socketDidDisconnect:(GCDAsyncSocket *)sock withError:(NSError *)err
{
    if (sock != listenSocket) {
        dispatch_async(dispatch_get_main_queue(), ^{
            NSLog(@"Client Disconnected");
        });
        @synchronized(connectedSockets) {
            [connectedSockets removeObject:sock];
        }
    }
}

@end


