//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"

@implementation Server

- (void) start {
    // Create server
    webServer = [[GCDWebServer alloc] init];
    [webServer addHandlerForMethod:@"POST"
                              path:@"/RPC2"
                      requestClass:[GCDWebServerDataRequest class]
                      processBlock:^GCDWebServerResponse *(GCDWebServerDataRequest* request) {
                          XMLRPCRequestDecoder *requestDecoder = [[XMLRPCRequestDecoder alloc]
                                                                  initWithData:[request data]];
                          NSLog(@"XML-RPC METHOD: %@", [requestDecoder method]);
                          NSLog(@"XML-RPC PARAMETERS: %@", [requestDecoder parameters]);

                          return [GCDWebServerDataResponse responseWithText:@"<xml></xml>"];
                      }];
    
    [webServer runWithPort:8082 bonjourName:nil];
}

- (void) stop {
    [webServer stop];
}

@end
