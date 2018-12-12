//
// Copyright (C) 2012 Eric Czarny <eczarny@gmail.com>
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>

@class XMLRPCDecoder;

@interface XMLRPCResponseDecoder : NSObject {
    NSString* myBody;
    id myObject;
    BOOL isFault;
}

- (id)initWithData:(NSData*)data;

- (BOOL)isFault;

- (NSNumber*)faultCode;

- (NSString*)faultString;

- (id)object;

- (NSString*)body;

- (NSString*)description;

@end
