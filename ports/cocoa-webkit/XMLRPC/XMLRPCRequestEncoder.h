//
// Copyright (C) 2012 Eric Czarny.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>

#import "XMLRPCEncoder.h"

@interface XMLRPCRequestEncoder : NSObject {
    NSMutableURLRequest* myRequest;
    id<XMLRPCEncoder> myXMLEncoder;
    NSTimeInterval myTimeout;
    id extra;
}

- (id)initWithURL:(NSURL*)URL;

- (void)setURL:(NSURL*)URL;

- (NSURL*)URL;

- (void)setUserAgent:(NSString*)userAgent;

- (NSString*)userAgent;

- (void)setEncoder:(id<XMLRPCEncoder>)encoder;

- (void)setMethod:(NSString*)method;

- (void)setMethod:(NSString*)method withParameter:(id)parameter;

- (void)setMethod:(NSString*)method withParameters:(NSArray*)parameters;

- (void)setTimeoutInterval:(NSTimeInterval)timeout;

- (NSString*)method;

- (NSArray*)parameters;

- (NSTimeInterval)timeout;

- (NSString*)body;

- (NSURLRequest*)request;

- (void)setValue:(NSString*)value forHTTPHeaderField:(NSString*)header;

- (id)extra;

- (void)setExtra:(id)extraObject;

@end
