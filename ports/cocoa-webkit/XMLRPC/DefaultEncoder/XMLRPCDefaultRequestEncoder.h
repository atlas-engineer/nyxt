//
// Copyright (C) 2012 Eric Czarny.
// Use of this file is governed by the license that can be found in LICENSE.
//

#import <Foundation/Foundation.h>
#import "XMLRPCEncoder.h"

@interface XMLRPCDefaultRequestEncoder : NSObject <XMLRPCEncoder> {
    NSString* myMethod;
    NSArray* myParameters;
}

@end
