//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"

@implementation Server

- (id)init {
    self = [super init];
    if (self) {
        webServer = [[GCDWebServer alloc] init];
        [webServer addHandlerForMethod:@"POST"
                                  path:@"/RPC2"
                          requestClass:[GCDWebServerDataRequest class]
                          processBlock:^GCDWebServerResponse*(GCDWebServerDataRequest* request) {
                              XMLRPCRequestDecoder* requestDecoder = [[XMLRPCRequestDecoder alloc]
                                  initWithData:[request data]];
                              NSLog(@"XML-RPC METHOD: %@", [requestDecoder method]);
                              NSLog(@"XML-RPC PARAMETERS: %@", [requestDecoder parameters]);

                              return [GCDWebServerDataResponse responseWithText:@"<xml></xml>"];
                          }];
    }
    return self;
}

- (void)start {
    [webServer runWithPort:8082 bonjourName:nil];
}

- (void)stop {
    [webServer stop];
}

@end
