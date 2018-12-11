//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import "GCDWebServer.h"
#import "GCDWebServerDataResponse.h"
#import "GCDWebServerDataRequest.h"
#include "Global.h"
#import "NextApplicationDelegate.h"
#import "XMLRPCRequestDecoder.h"

@interface Server : NSObject
{
    GCDWebServer* webServer;
}

- (void) start;
- (void) stop;

@end
