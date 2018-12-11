//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import "XMLRPCRequestEncoder.h"
#import "XMLRPCDecoder.h"
#import "XMLRPCDefaultRequestDecoder.h"
#import "XMLRPCDefaultRequestEncoder.h"
#import "XMLRPCEncoder.h"

static const NSTimeInterval DEFAULT_TIMEOUT = 240;

@implementation XMLRPCRequestEncoder

- (id)initWithURL:(NSURL*)URL withEncoder:(id<XMLRPCEncoder>)encoder {
    self = [super init];
    if (self) {
        if (URL) {
            myRequest = [[NSMutableURLRequest alloc] initWithURL:URL];
        } else {
            myRequest = [[NSMutableURLRequest alloc] init];
        }
        myXMLEncoder = encoder;
        myTimeout = DEFAULT_TIMEOUT;
    }

    return self;
}

- (id)initWithURL:(NSURL*)URL {
    return [self initWithURL:URL withEncoder:[[XMLRPCDefaultRequestEncoder alloc] init]];
}

- (void)setURL:(NSURL*)URL {
    [myRequest setURL:URL];
}

- (NSURL*)URL {
    return [myRequest URL];
}

- (void)setUserAgent:(NSString*)userAgent {
    if (![self userAgent]) {
        [myRequest addValue:userAgent forHTTPHeaderField:@"User-Agent"];
    } else {
        [myRequest setValue:userAgent forHTTPHeaderField:@"User-Agent"];
    }
}

- (NSString*)userAgent {
    return [myRequest valueForHTTPHeaderField:@"User-Agent"];
}

- (void)setEncoder:(id<XMLRPCEncoder>)encoder {
    NSString* method = [myXMLEncoder method];
    NSArray* parameters = [myXMLEncoder parameters];
    myXMLEncoder = encoder;
    [myXMLEncoder setMethod:method withParameters:parameters];
}

- (void)setMethod:(NSString*)method {
    [myXMLEncoder setMethod:method withParameters:nil];
}

- (void)setMethod:(NSString*)method withParameter:(id)parameter {
    NSArray* parameters = nil;

    if (parameter) {
        parameters = [NSArray arrayWithObject:parameter];
    }

    [myXMLEncoder setMethod:method withParameters:parameters];
}

- (void)setMethod:(NSString*)method withParameters:(NSArray*)parameters {
    [myXMLEncoder setMethod:method withParameters:parameters];
}

- (void)setTimeoutInterval:(NSTimeInterval)timeout {
    myTimeout = timeout;
}

- (NSString*)method {
    return [myXMLEncoder method];
}

- (NSArray*)parameters {
    return [myXMLEncoder parameters];
}

- (NSTimeInterval)timeout {
    return myTimeout;
}

- (NSString*)body {
    return [myXMLEncoder encode];
}

- (NSURLRequest*)request {
    NSData* content = [[self body] dataUsingEncoding:NSUTF8StringEncoding];
    NSNumber* contentLength = [NSNumber numberWithUnsignedInteger:[content length]];

    if (!myRequest) {
        return nil;
    }

    [myRequest setHTTPMethod:@"POST"];

    if (![myRequest valueForHTTPHeaderField:@"Content-Type"]) {
        [myRequest addValue:@"text/xml" forHTTPHeaderField:@"Content-Type"];
    } else {
        [myRequest setValue:@"text/xml" forHTTPHeaderField:@"Content-Type"];
    }

    if (![myRequest valueForHTTPHeaderField:@"Content-Length"]) {
        [myRequest addValue:[contentLength stringValue] forHTTPHeaderField:@"Content-Length"];
    } else {
        [myRequest setValue:[contentLength stringValue] forHTTPHeaderField:@"Content-Length"];
    }

    if (![myRequest valueForHTTPHeaderField:@"Accept"]) {
        [myRequest addValue:@"text/xml" forHTTPHeaderField:@"Accept"];
    } else {
        [myRequest setValue:@"text/xml" forHTTPHeaderField:@"Accept"];
    }

    if (![self userAgent]) {
        NSString* userAgent = [[NSUserDefaults standardUserDefaults] objectForKey:@"UserAgent"];

        if (userAgent) {
            [self setUserAgent:userAgent];
        }
    }

    [myRequest setHTTPBody:content];

    return (NSURLRequest*)myRequest;
}

- (void)setValue:(NSString*)value forHTTPHeaderField:(NSString*)header {
    [myRequest setValue:value forHTTPHeaderField:header];
}

- (id)extra {
    return extra;
}

- (void)setExtra:(id)extraObject {
    extra = extraObject;
}

@end
