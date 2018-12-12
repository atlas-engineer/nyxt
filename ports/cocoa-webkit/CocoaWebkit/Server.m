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
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate windowMake];});
                 responseEncoder = [[XMLRPCResponseEncoder alloc]
                                    initWithParameters:@[operationResult]];
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
             }
             else if ([method isEqualToString:@"window.active"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate windowActive];});
             }
             else if ([method isEqualToString:@"window.exists"]) {
                 __block bool operationResult;
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate windowExists: [parameters objectAtIndex:0]];});
             }
             else if ([method isEqualToString:@"window.set.active.buffer"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate setActiveBufferForWindow: [parameters objectAtIndex:0]
                                                 buffer: [parameters objectAtIndex:1]];});
             }
             else if ([method isEqualToString:@"window.set.minibuffer.height"]) {
                 __block NSInteger operationResult = 0;
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate setMinibufferHeightForWindow: [parameters objectAtIndex:0]
                                                                       height: [parameters objectAtIndex:1]];});
             }
             else if ([method isEqualToString:@"buffer.make"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate bufferMake];});
             }
             else if ([method isEqualToString:@"buffer.delete"]) {
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     [delegate bufferDelete: [parameters objectAtIndex:0]];});
             }
             else if ([method isEqualToString:@"buffer.evaluate.javascript"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate bufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                               javaScript:[parameters objectAtIndex:1]];});
             }
             else if ([method isEqualToString:@"minibuffer.evaluate.javascript"]) {
                 __block NSString* operationResult = @"";
                 dispatch_sync(dispatch_get_main_queue(), ^{
                     NextApplicationDelegate *delegate = [NSApp delegate];
                     operationResult = [delegate minibufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                                   javaScript:[parameters objectAtIndex:1]];});
             }
             
             responseEncoder = [[XMLRPCResponseEncoder alloc]
                                initWithParameters:@[@"Unsupported operation!"]];
             return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
         }];
    }
    return self;
}

- (void)start {
    [webServer startWithPort:8082 bonjourName:nil];
}

- (void)stop {
    [webServer stop];
}

@end
