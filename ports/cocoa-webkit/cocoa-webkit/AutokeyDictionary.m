//
//  AutokeyDictionary.m
//  cocoa-webkit
//
//  Created by John Mercouris on 3/14/18.
//  Copyright © 2018 Atlas Engineer LLC. All rights reserved.
//

#import "AutokeyDictionary.h"

@implementation AutokeyDictionary
@synthesize elementCount;

- (instancetype) init
{
    self = [super init];
    if (self)
    {
        [self setElementCount:0];
        _dict = [[NSMutableDictionary alloc] init];
    }
    return self;
}

- (NSString *) insertElement:(NSObject *) object
{
    NSString *elementKey = [@([self elementCount]) stringValue];
    [_dict setValue:object forKey: elementKey];
    [self setElementCount:[self elementCount] + 1];
    return elementKey;
}

- (NSUInteger)count {
    return [_dict count];
}

- (id)objectForKey:(id)aKey {
    return [_dict objectForKey:aKey];
}

- (void)removeObjectForKey:(id)aKey {
    [_dict removeObjectForKey:aKey];
}

- (NSEnumerator *)keyEnumerator {
    return [_dict keyEnumerator];
}

- (NSArray*)allKeys {
    return [_dict allKeys];
}

@end
