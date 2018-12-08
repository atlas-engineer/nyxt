//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"
#import "NextApplicationDelegate.h"

@implementation Server

- (id) init {
    self = [super init];
    if (self != nil) {
        socket = [[GCDAsyncSocket alloc]
                  initWithDelegate:self delegateQueue:dispatch_get_main_queue()];
    }
    return self;
}

-(void) start {
    NSError *err;
    [socket connectToHost:@"127.0.0.1" onPort:8080 error:&err];
    NSLog(@"%@", err);
    NSLog(@"Server Started.");
}

- (void)onSocket:(GCDAsyncSocket *)sock didReadData:(NSData *)data withTag:(long)tag
{
    NSData *strData = [data subdataWithRange:NSMakeRange(0, [data length])];
    NSString *msg = [[NSString alloc] initWithData:strData encoding:NSUTF8StringEncoding];
    if(msg)
    {
        NSLog(@"RX:%@",msg);
    }
}

- (void)onSocket:(GCDAsyncSocket *)sock willDisconnectWithError:(NSError *)err
{
    NSLog(@"error - disconnecting");
    //you'd probably want to start reconnecting procedure here...
}

- (void)onSocketDidDisconnect:(GCDAsyncSocket *)sock
{
    NSLog(@"disconnected");
}

- (void)onSocket:(GCDAsyncSocket *)sock didConnectToHost:(NSString *)host port:(UInt16)port
{
    NSLog(@"connected");
}

@end
