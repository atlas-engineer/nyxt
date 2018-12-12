//
// Copyright © 2017-2018 Atlas Engineer LLC.
// Use of this file is governed by the license that can be found in LICENSE.
//  

#import <Foundation/Foundation.h>

#import "XMLRPCEncoder.h"


@interface XMLRPCResponseEncoder : NSObject {
    id<XMLRPCEncoder> myXMLEncoder;
}

- (id)initWithParameters:(NSArray*)parameters;

- (void)setEncoder:(id<XMLRPCEncoder>)encoder;

- (void)setParameters:(NSArray*)parameters;

- (NSArray*)parameters;

- (NSString*)body;

@end
