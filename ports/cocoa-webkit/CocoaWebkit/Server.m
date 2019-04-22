//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "Server.h"

@implementation Server
@synthesize XMLRPCMethods;

- (id)init {
    self = [super init];
    if (self) {
        webServer = [[GCDWebServer alloc] init];
        delegate = [NSApp delegate];
        [self setXMLRPCMethods:[[NSMutableDictionary alloc] init]];
        [self registerMethod:@"window.make" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate windowMake:[parameters objectAtIndex:0]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"window.set.title" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate window: [parameters objectAtIndex:0]
                        setTitle: [parameters objectAtIndex:1]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"window.delete" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate windowDelete: [parameters objectAtIndex:0]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"window.active" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            __block NSString* operationResult = @"";
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                operationResult = [delegate windowActive];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[operationResult]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"window.exists" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
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
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[objectResult]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];

        }];
        [self registerMethod:@"window.set.active.buffer" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate setActiveBufferForWindow: [parameters objectAtIndex:0]
                                            buffer: [parameters objectAtIndex:1]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];

        }];
        [self registerMethod:@"window.set.minibuffer.height" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            __block NSNumber *operationResult;
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                operationResult = [NSNumber numberWithInt:[delegate setMinibufferHeightForWindow: [parameters objectAtIndex:0]
                                                                                          height: [parameters objectAtIndex:1]]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[operationResult]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];

        }];
        [self registerMethod:@"buffer.make" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate bufferMake: [parameters objectAtIndex:0]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];

        }];
        [self registerMethod:@"buffer.delete" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate bufferDelete: [parameters objectAtIndex:0]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"buffer.load" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                [delegate bufferLoad:[parameters objectAtIndex:0]
                                 url:[parameters objectAtIndex:1]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"buffer.evaluate.javascript" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            __block NSString* operationResult = @"";
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                operationResult = [delegate bufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                          javaScript:[parameters objectAtIndex:1]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[operationResult]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"minibuffer.evaluate.javascript" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            __block NSString* operationResult = @"";
            dispatch_sync(dispatch_get_main_queue(), ^{
                NextApplicationDelegate *delegate = [NSApp delegate];
                operationResult = [delegate minibufferEvaluateJavaScript:[parameters objectAtIndex:0]
                                                              javaScript:[parameters objectAtIndex:1]];});
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[operationResult]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"generate.input.event" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            NSMutableArray* lowLevelData = [parameters objectAtIndex: 3];
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
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                               initWithParameters:@[@YES]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        [self registerMethod:@"listMethods" withBlock:^GCDWebServerDataResponse *(NSArray *parameters) {
            XMLRPCResponseEncoder* responseEncoder = [[XMLRPCResponseEncoder alloc]
                                                      initWithParameters:@[[self listMethods]]];
            return [GCDWebServerDataResponse responseWithText:[responseEncoder body]];
        }];
        
        __unsafe_unretained typeof(self) weakSelf = self;
        [webServer
         addHandlerForMethod:@"POST"
         path:@"/RPC2"
         requestClass:[GCDWebServerDataRequest class]
         processBlock:^GCDWebServerResponse*(GCDWebServerDataRequest* request) {
             XMLRPCRequestDecoder *requestDecoder = [[XMLRPCRequestDecoder alloc]
                                                     initWithData:[request data]];
             NSString *method = [requestDecoder method];
             NSArray *parameters = [requestDecoder parameters];
             return [weakSelf executeMethod:method withParameters:parameters];
         }];
    }
    return self;
}
- (void)registerMethod:(NSString*)methodName withBlock:(GCDWebServerDataResponse* (^)(NSArray*))executionBlock {
    [[self XMLRPCMethods] setValue:executionBlock forKey:methodName];
}

- (GCDWebServerDataResponse*)executeMethod:(NSString*)methodName withParameters:(NSArray*)parameters {
    GCDWebServerDataResponse*(^block)(NSArray*) = [[self XMLRPCMethods] objectForKey:methodName];
    return block(parameters);
}

- (NSArray*)listMethods {
    return [[self XMLRPCMethods] allKeys];
}

- (void)start {
    [webServer startWithPort:[[[Global sharedInstance] port] intValue] bonjourName:nil];
}

- (void)stop {
    [webServer stop];
}

@end
