//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"
#import "NextApplicationDelegate.h"
#include "Global.h"
#import "GCDWebServer.h"
#import "GCDWebServerDataResponse.h"

@implementation Server

- (void) start {
    @autoreleasepool {
        // Create server
        GCDWebServer* webServer = [[GCDWebServer alloc] init];
        
        // Add a handler to respond to GET requests on any URL
        [webServer addDefaultHandlerForMethod:@"GET"
                                 requestClass:[GCDWebServerRequest class]
                                 processBlock:^GCDWebServerResponse *(GCDWebServerRequest* request) {
                                     return [GCDWebServerDataResponse responseWithHTML:
                                             @"<html><body><p>Hello World</p></body></html>"];
                                 }];
        
        // Use convenience method that runs server on port 8080
        // until SIGINT (Ctrl-C in Terminal) or SIGTERM is received
        [webServer runWithPort:8080 bonjourName:nil];
        NSLog(@"Visit %@ in your web browser", webServer.serverURL);
    }
}

- (void) stop {
    
}

@end
