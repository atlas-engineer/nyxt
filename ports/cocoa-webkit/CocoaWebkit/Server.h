//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "GCDWebServer.h"
#import "GCDWebServerDataRequest.h"
#import "GCDWebServerDataResponse.h"
#include "Global.h"
#import "NextApplicationDelegate.h"
#import "XMLRPCRequestDecoder.h"
#import <Foundation/Foundation.h>

@interface Server : NSObject {
    GCDWebServer* webServer;
}

- (void)start;
- (void)stop;

@end
