//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//


#import "NextApplication.h"
#include "Global.h"
#include "NextApplicationDelegate.h"
#include "XMLRPCRequestEncoder.h"
#include "XMLRPCResponseDecoder.h"

@implementation NextApplication

- (id)init {
    self = [super init];
    if (self) {
        NSAppleEventManager *em = [NSAppleEventManager sharedAppleEventManager];
        [em
         setEventHandler:self
         andSelector:@selector(getUrl:withReplyEvent:)
         forEventClass:kInternetEventClass
         andEventID:kAEGetURL];
    }
    return self;
}

+ (NSString*)mapCharactersForEventWithCharacters:(NSString*)characters  keyCode:(short)code {
    // From the documentation on key codes: The property’s value is hardware-independent.
    switch (code) {
        case 27: return @"HYPHEN";
        case 36: return @"RETURN";
        case 48: return @"TAB";
        case 49: return @"SPACE";
        case 51: return @"BACKSPACE";
        case 53: return @"ESCAPE";
        case 117: return @"DELETE";
        case 123: return @"Left";
        case 124: return @"Right";
        case 125: return @"Down";
        case 126: return @"Up";
    }
    return characters;
}

- (void)sendEvent:(NSEvent*)event {
    if ([event type] == NSEventTypeKeyDown && [event timestamp] != 0) {
        NextApplicationDelegate* delegate = [NSApp delegate];
        NSString* activeWindow = [delegate windowActive];
        NSEventModifierFlags modifierFlags = [event modifierFlags];
        NSNumber* keyCode = [NSNumber numberWithShort:[event keyCode]];
        NSString* characters = [NextApplication mapCharactersForEventWithCharacters:[event charactersIgnoringModifiers]
                                                                            keyCode:[event keyCode]];
        NSMutableArray* modifiers = [[NSMutableArray alloc] init];
        if (modifierFlags & NSEventModifierFlagControl) {
            [modifiers addObject:@"C"];
        }
        if (modifierFlags & NSEventModifierFlagOption) {
            [modifiers addObject:@"M"];
        }
        if (modifierFlags & NSEventModifierFlagCommand) {
            [modifiers addObject:@"S"];
        }
        if (modifierFlags & NSEventModifierFlagShift) {
            [modifiers addObject:@"s"];
        }

        NSString* coreSocket = [[Global sharedInstance] coreSocket];
        XMLRPCRequestEncoder* request = [[XMLRPCRequestEncoder alloc]
                                         initWithURL:[NSURL URLWithString:coreSocket]];
        [request setMethod:@"PUSH-KEY-EVENT"
            withParameters:@[ keyCode, characters, modifiers, activeWindow ]];
        NSURLSession* session = [NSURLSession sessionWithConfiguration:[NSURLSessionConfiguration defaultSessionConfiguration]];
        
        [[session dataTaskWithRequest:[request request]
                    completionHandler:^(NSData* data, NSURLResponse* response, NSError* error) {
                        XMLRPCResponseDecoder* RPCResponse = [[XMLRPCResponseDecoder alloc] initWithData:data];
                        // When Not Consumed, send the event to the Cocoa Application
                        if ([[RPCResponse object] intValue] == 0) {
                            dispatch_sync(dispatch_get_main_queue(), ^{
                                [super sendEvent:[NSEvent keyEventWithType:NSEventTypeKeyDown
                                                                        location:[event locationInWindow]
                                                                   modifierFlags:[event modifierFlags]
                                                                       timestamp:0
                                                                    windowNumber:[event windowNumber]
                                                                         context:nil
                                                                      characters:[event characters]
                                                     charactersIgnoringModifiers:[event charactersIgnoringModifiers]
                                                                       isARepeat:[event isARepeat]
                                                                         keyCode:[event keyCode]]];
                            });
                        } else {
                            [request setMethod:@"CONSUME-KEY-SEQUENCE" withParameters:@[ activeWindow ]];
                            [[session dataTaskWithRequest:[request request]] resume];
                        }
                    }] resume];

    } else {
        [super sendEvent:event];
    }
}

- (void)getUrl:(NSAppleEventDescriptor *)event
withReplyEvent:(NSAppleEventDescriptor *)replyEvent
{
    // Get the URL
    NSString *url = [[event paramDescriptorForKeyword:keyDirectObject]
                        stringValue];
    NSString* coreSocket = [[Global sharedInstance] coreSocket];
    XMLRPCRequestEncoder* request = [[XMLRPCRequestEncoder alloc] initWithURL:
                                     [NSURL URLWithString:coreSocket]];
    // Make Buffers Expects an array of URLs
    [request setMethod:@"MAKE-BUFFERS"
        withParameters:@[@[url]]];
    NSURLSession* session = [NSURLSession sharedSession];
    [[session dataTaskWithRequest:[request request]] resume];
}

@end
