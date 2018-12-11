//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"
#include "Global.h"
#import "NextApplicationDelegate.h"
#import "GCDWebServer.h"
#import "GCDWebServerDataResponse.h"
#import "GCDWebServerDataRequest.h"
#import "XMLRPCRequestDecoder.h"

@implementation Server

- (void) start {
    // Create server
    GCDWebServer* webServer = [[GCDWebServer alloc] init];
    [webServer addHandlerForMethod:@"POST"
                              path:@"/RPC2"
                      requestClass:[GCDWebServerDataRequest class]
                      processBlock:^GCDWebServerResponse *(GCDWebServerDataRequest* request) {
                          XMLRPCRequestDecoder *requestDecoder = [[XMLRPCRequestDecoder alloc]
                                                                  initWithData:[request data]];
                          NSLog(@"XML-RPC METHOD: %@", [requestDecoder method]);
                          return [GCDWebServerDataResponse responseWithText:@"<xml></xml>"];
                      }];
    
    // Use convenience method that runs server on port 8080
    // until SIGINT (Ctrl-C in Terminal) or SIGTERM is received
    [webServer runWithPort:8082 bonjourName:nil];
    NSLog(@"Visit %@ in your web browser", webServer.serverURL);
}

- (void) stop {
    
}

@end
