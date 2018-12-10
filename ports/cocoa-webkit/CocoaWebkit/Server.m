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
#import "XMLRPCResponseDecoder.h"

@implementation Server

- (void) start {
    // Create server
    GCDWebServer* webServer = [[GCDWebServer alloc] init];
    [webServer addHandlerForMethod:@"POST"
                              path:@"/RPC2"
                      requestClass:[GCDWebServerDataRequest class]
                      processBlock:^GCDWebServerResponse *(GCDWebServerDataRequest* request) {
                          XMLRPCResponseDecoder *response = [[XMLRPCResponseDecoder alloc]
                                                      initWithData:[request data]];
                          NSLog(@"XMLRPC Description: %@", [response description]);
                          return [GCDWebServerDataResponse responseWithText:@"<xml>"];
                      }];
    
    // Use convenience method that runs server on port 8080
    // until SIGINT (Ctrl-C in Terminal) or SIGTERM is received
    [webServer runWithPort:8082 bonjourName:nil];
    NSLog(@"Visit %@ in your web browser", webServer.serverURL);
}

- (void) stop {
    
}

@end
