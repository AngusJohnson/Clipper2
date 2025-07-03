#ifndef CLIPPER_ALLOCATOR_H
#define CLIPPER_ALLOCATOR_H

#include <memory>

namespace Clipper2Lib {

#ifdef clipper2_custom_allocator

	thread_local extern void *clipper2_allocator_userp;
	extern void* (*clipper2_malloc)(void *userp, std::size_t n);
	extern void  (*clipper2_free)(void *userp, void* p);

	template <class T>
	struct Allocator
	{
		using value_type = T;

		Allocator() : userp(clipper2_allocator_userp) {}

		template<class U>
		constexpr Allocator(const Allocator <U>&) noexcept {}

		void *userp = NULL;
		T* allocate(std::size_t n) { return (T*)clipper2_malloc(userp, n * sizeof(T)); }
		void deallocate(T* p, std::size_t) { clipper2_free(userp, (void*)p); }
	};

	template <class T, class U>
	bool operator==(const Allocator <T>&, const Allocator <U>&) { return true; }
	template <class T, class U>
	bool operator!=(const Allocator <T>&, const Allocator <U>&) { return false; }

#else

	template <class T>
	using Allocator = std::allocator<T>;

#endif

	template <class T, class... Args>
	static T* New(Args&&... args)
	{
		auto a = Allocator<T>{};
		T* p = a.allocate(1);
		std::allocator_traits<Allocator<T>>::construct(a, p, std::forward<Args>(args)...);
		return p;
	}

	template <class T>
	void Delete(T* p)
	{
		auto a = Allocator<T>{};
		std::allocator_traits<Allocator<T>>::destroy(a, p);
		a.deallocate(p, 1);
	}

	template <class T, class... Args>
	static T* NewArray(std::size_t n)
	{
		auto a = Allocator<T>{};
		T* p = a.allocate(n);
		for (size_t i = 0; i < n; ++i)
			std::allocator_traits<Allocator<T>>::construct(a, &p[i]);
		return p;
	}

	template <class T>
	void DeleteArray(T* p, std::size_t n)
	{
		auto a = Allocator<T>{};
		for (size_t i = 0; i < n; ++i)
			std::allocator_traits<Allocator<T>>::destroy(a, &p[i]);
		a.deallocate(p, n);
	}

	// UniquePtr
	template <class T>
	struct Deleter
	{
		void operator()(T* p) const { Delete(p); }
	};

	template <class T>
	using UniquePtr = std::unique_ptr<T, Deleter<T>>;

	template <class T, class... Args>
	static UniquePtr<T> MakeUnique(Args&&... args) { T* p = New<T>(std::forward<Args>(args)...); return UniquePtr<T>(p); }
}  // namespace

#endif  // CLIPPER_ALLOCATOR_H
