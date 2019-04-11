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
        delegate = [NSApp delegate];
        [webServer
         addHandlerForMethod:@"POST"
         path:@"/RPC2"
         requestClass:[GCDWebServerDataRequest class]
         processBlock:^GCDWebServerResponse*(GCDWebServerDataRequest* request) {
             XMLRPCRequestDecoder *requestDecoder = [[XMLRPCRequestDecoder alloc]
                                                     initWithData:[request data]];
             XMLRPCResponseEncoder *responseEncoder;
             NSString *method = [requestDecoder method];
             NSArray *parameters = [requestDecoder parameters];
             
             if ([method isEqualToString:@"window.make"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate windowMake:[parameters objectAtIndex:0]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.set.title"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate window: [parameters objectAtIndex:0]
                             setTitle: [parameters objectAtIndex:1]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.delete"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate windowDelete: [parameters objectAtIndex:0]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.active"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate windowActive];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[operationResult]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.exists"]) {
                 __block bool operationResult;
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate windowExists: [parameters objectAtIndex:0]];});
                 NSObject *objectResult;
                 if (operationResult) {
                     objectResult = @YES;
                 } else {
                     objectResult = @NO;
                 }
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[objectResult]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.set.active.buffer"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate setActiveBufferForWindow: [parameters objectAtIndex:0]
                                                 buffer: [parameters objectAtIndex:1]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"window.set.minibuffer.height"]) {
                 __block NSNumber *operationResult;
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [NSNumber numberWithInt:[delegate setMinibufferHeightForWindow: [parameters objectAtIndex:0]
                                                                                               height: [parameters objectAtIndex:1]]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[operationResult]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"buffer.make"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate bufferMake: [parameters objectAtIndex:0]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"buffer.delete"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate bufferDelete: [parameters objectAtIndex:0]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"buffer.evaluate.javascript"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate bufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                               javaScript:[parameters objectAtIndex:1]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[operationResult]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"minibuffer.evaluate.javascript"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate minibufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                                   javaScript:[parameters objectAtIndex:1]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[operationResult]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }
             else if ([method isEqualToString:@"generate.input.event"]) {
                 NSMutableArray* lowLevelData = [parameters objectAtIndex: 3];
                 NSLog(@"Low Level Data: %@", lowLevelData);
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     [NSApp sendEvent:[NSEvent keyEventWithType:NSEventTypeKeyDown
                                                       location:CGPointZero
                                                  modifierFlags:[[lowLevelData objectAtIndex:0] integerValue]
                                                      timestamp:0
                                                   windowNumber:(NSInteger)[lowLevelData objectAtIndex:1]
                                                        context:nil
                                                     characters:[lowLevelData objectAtIndex:2]
                                    charactersIgnoringModifiers:[lowLevelData objectAtIndex:3]
                                                      isARepeat:NO
                                                        keyCode:(short)[parameters objectAtIndex: 1]]];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[@YES]];
                 return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
             }

             responseEncoder = [[XMLRPCResponseEncoder alloc]
                                initWithParameters:@[@"Unsupported operation!"]];
             return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
         }];
    }
    return self;
}

- (void)start {
    [webServer startWithPort:[[[Global sharedInstance] port] intValue] bonjourName:nil];
}

- (void)stop {
    [webServer stop];
}

@end
